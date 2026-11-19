# Script 1: limpieza de datos y construcción de variables
#
# Aquí armamos la base municipio-elección (2018, 2021 y 2024) con:
#   - el NEC (variable dependiente), a partir de los resultados del INE
#   - la violencia del crimen organizado (UCDP)
#   - la violencia político-criminal (Votar entre Balas)
#   - la violencia en los municipios vecinos (para la H2)
#   - la lista nominal, que usamos como control de tamaño
#
# Al final queda guardada en data/procesados/base_analisis.csv

library(dplyr)
library(tidyr)
library(readr)
library(purrr)
library(sf)
library(spdep)

dir.create("data/procesados", showWarnings = FALSE)
dir.create("data/inegi", showWarnings = FALSE)


# 1. Ventanas de tiempo
# Contamos la violencia en los 6 meses previos a cada elección (como dice Gómez Díaz, 2025)

ventanas <- tibble(
  eleccion = c(2018, 2021, 2024),
  inicio   = as.Date(c("2018-01-01", "2020-12-06", "2023-12-02")),
  fin      = as.Date(c("2018-06-30", "2021-06-05", "2024-06-01"))
)


# 2. Resultados electorales (INE) 
# Los zips del SICEE pesan mucho y no están en el repositorio. Si están en
# data/ine/<año>/, sacamos de cada uno solo el archivo de ayuntamientos por
# municipio y candidatura (*_MUNCAND.csv). Si ya hicimos ese paso antes,
# usamos directamente data/ine/ine_muncand_largo.csv.

archivo_ine <- "data/ine/ine_muncand_largo.csv"

if (!file.exists(archivo_ine)) {
  zips <- list.files("data/ine", pattern = "\\.zip$", recursive = TRUE, full.names = TRUE)
  ine_largo <- list()

  for (z in zips) {
    anio <- as.numeric(basename(dirname(z)))
    archivos <- unzip(z, list = TRUE)$Name
    muncand <- archivos[grepl("AYUN.*MUNCAND.*\\.csv$", archivos)]
    if (length(muncand) == 0) next   #ese estado no eligió ayuntamientos ese año

    ruta <- unzip(z, files = muncand[1], exdir = tempdir(), junkpaths = TRUE)
    datos <- read_csv(ruta, col_types = cols(.default = "c"),
                      locale = locale(encoding = "latin1"))
    names(datos) <- toupper(trimws(names(datos)))

    #las columnas de candidaturas son las que no son identificadores ni totales
    no_cand <- "^(CIRCUNSCRIPCION|ID_|NOMBRE_|CABECERA|MUNICIPIO|SECCION|CASILLA|ACTA|NUM_VOTOS|TOTAL_VOTOS|LISTA_NOMINAL|RUTA|OBSERVACION)"
    cand <- names(datos)[!grepl(no_cand, names(datos))]

    ine_largo[[z]] <- datos |>
      mutate(anio = anio, zip = basename(z)) |>
      select(anio, zip, ID_ESTADO, ID_MUNICIPIO, MUNICIPIO, NUM_VOTOS_VALIDOS,
             TOTAL_VOTOS, LISTA_NOMINAL, all_of(cand)) |>
      pivot_longer(all_of(cand), names_to = "candidatura", values_to = "votos") |>
      mutate(votos = parse_number(votos)) |>
      filter(!is.na(votos))   #celda vacía significa que esa candidatura no compitió ahí
  }
  write_csv(bind_rows(ine_largo), archivo_ine)
}

#aquí ojo: na = "" para que Nueva Alianza ("NA") no se lea como dato faltante
ine <- read_csv(archivo_ine, na = "", col_types = cols(.default = "c")) |>
  mutate(across(c(anio, ID_ESTADO, ID_MUNICIPIO, LISTA_NOMINAL, votos), as.numeric),
         estado = sub("_.*", "", zip))


# 3. NEC: número efectivo de candidaturas
# NEC = 1 / suma(p^2), con p = proporción de votos de cada candidatura (Laakso y Taagepera, 1979)
# En algunos estados el INE reparte los votos de una coalición en varias
# columnas (PVEM, PT, MORENA, PVEM_PT, etc.). Como un partido no puede tener dos
# candidaturas en el mismo municipio, juntamos todas las columnas que
# comparten algún partido y las contamos como una sola candidatura.

juntar_coaliciones <- function(candidaturas, votos) {
  grupos <- list()
  for (i in seq_along(candidaturas)) {
    if (grepl("^(CAND_IND|CI)", candidaturas[i])) {
      partidos <- candidaturas[i]   #cada independiente es su propia candidatura
    } else {
      partidos <- strsplit(sub("^C_", "", candidaturas[i]), "_")[[1]]
      partidos <- partidos[!grepl("^[0-9]+$", partidos)]
    }
    total <- votos[i]
    #si comparte partidos con un grupo que ya teníamos, se fusionan
    for (g in rev(seq_along(grupos))) {
      if (any(partidos %in% grupos[[g]]$partidos)) {
        partidos <- union(partidos, grupos[[g]]$partidos)
        total <- total + grupos[[g]]$votos
        grupos[[g]] <- NULL
      }
    }
    grupos[[length(grupos) + 1]] <- list(partidos = partidos, votos = total)
  }
  sapply(grupos, function(g) g$votos)
}

calcular_nec <- function(v) 1 / sum((v / sum(v))^2)

nec <- ine |>
  filter(votos > 0) |>
  group_by(anio, estado, ID_ESTADO, ID_MUNICIPIO) |>
  summarise(MUNICIPIO = first(MUNICIPIO),
            lista_nominal = first(LISTA_NOMINAL),
            votos_cand = list(juntar_coaliciones(candidatura, votos)),
            .groups = "drop") |>
  mutate(n_candidaturas = lengths(votos_cand),
         nec = sapply(votos_cand, calcular_nec)) |>
  select(-votos_cand)

#municipios que quedaron fuera por no tener votos (ej. Cherán, por usos y costumbres)
excluidos <- ine |>
  distinct(anio, estado, ID_ESTADO, ID_MUNICIPIO, MUNICIPIO) |>
  anti_join(nec, by = c("anio", "ID_ESTADO", "ID_MUNICIPIO"))
write_csv(excluidos, "data/procesados/exclusiones_nec.csv")
cat("Municipio-elecciones con NEC:", nrow(nec), "| excluidos:", nrow(excluidos), "\n")


# 4. Mapa municipal del INEGI 
# Lo bajamos del servicio web del INEGI (un estado a la vez). Solo la primera vez

archivo_mapa <- "data/inegi/municipios_inegi.gpkg"

if (!file.exists(archivo_mapa)) {
  url <- "https://gaia.inegi.org.mx/wscatgeo/v2/geo/mgem/"
  mapa <- map(sprintf("%02d", 1:32), ~ st_read(paste0(url, .x), quiet = TRUE)) |>
    bind_rows()
  st_write(mapa, archivo_mapa, quiet = TRUE)
}

mapa <- st_read(archivo_mapa, quiet = TRUE) |>
  transmute(cve_ent = as.integer(cve_ent), cve_mun = as.integer(cve_mun),
            nombre = nomgeo, pob_2020 = as.numeric(pob_total)) |>
  st_make_valid()

catalogo <- st_drop_geometry(mapa)


# 5. Violencia del crimen organizado (UCDP)
# data/ucdp/ucdp_ged_mexico.csv sale de la base global GED 26.1 filtrada con
# subset(ged, country == "Mexico").
# Filtros: violencia no estatal (2) y unilateral (3), 
# ubicación al menos municipal (where_prec <= 3), 
# fecha al menos al mes (date_prec <= 4) y 
# sin eventos donde participa el gobierno de México (porque no es el objeto de estudio)

sf_use_s2(FALSE)

ucdp <- read_csv("data/ucdp/ucdp_ged_mexico.csv", show_col_types = FALSE) |>
  mutate(fecha = as.Date(substr(as.character(date_start), 1, 10))) |>
  filter(type_of_violence %in% c(2, 3),
         where_prec <= 3,
         date_prec <= 4,
         !grepl("Government", side_a),
         !grepl("Government", side_b)) |>
  inner_join(ventanas, join_by(between(fecha, inicio, fin)))

#ubicamos cada evento en su municipio con las coordenadas
#(si cae justo fuera, por ejemplo en la costa, va al municipio más cercano)
puntos <- st_as_sf(ucdp, coords = c("longitude", "latitude"), crs = 4326)
puntos <- st_transform(puntos, st_crs(mapa))
cual <- st_nearest_feature(puntos, mapa)
ucdp$cve_ent <- mapa$cve_ent[cual]
ucdp$cve_mun <- mapa$cve_mun[cual]

write_csv(select(ucdp, id, eleccion, fecha, best, side_a, side_b, latitude, longitude,
                 cve_ent, cve_mun),
          "data/procesados/ucdp_eventos_municipio.csv")
cat("Eventos UCDP en las ventanas:", nrow(ucdp), "\n")

#conteo para todos los municipios del país (los que no tienen eventos quedan en 0)
violencia <- expand_grid(select(catalogo, cve_ent, cve_mun), eleccion = ventanas$eleccion) |>
  left_join(count(ucdp, cve_ent, cve_mun, eleccion, name = "ucdp_eventos"),
            by = c("cve_ent", "cve_mun", "eleccion")) |>
  mutate(ucdp_eventos = replace_na(ucdp_eventos, 0))


# 6. Violencia en los municipios vecinos (H2)
# vecinos = municipios que comparten límite

lista_vecinos <- poly2nb(mapa, queen = TRUE, snap = 1e-6)
vecinos <- map_dfr(seq_along(lista_vecinos), function(i) {
  j <- lista_vecinos[[i]][lista_vecinos[[i]] > 0]
  tibble(cve_ent = mapa$cve_ent[i], cve_mun = mapa$cve_mun[i],
         vecino_ent = mapa$cve_ent[j], vecino_mun = mapa$cve_mun[j])
})
write_csv(vecinos, "data/procesados/vecinos_municipios.csv")

promedio_vecinos <- vecinos |>
  inner_join(rename(violencia, vecino_ent = cve_ent, vecino_mun = cve_mun),
             by = c("vecino_ent", "vecino_mun"), relationship = "many-to-many") |>
  group_by(cve_ent, cve_mun, eleccion) |>
  summarise(ucdp_vecinos = mean(ucdp_eventos), .groups = "drop")

violencia <- violencia |>
  left_join(promedio_vecinos, by = c("cve_ent", "cve_mun", "eleccion")) |>
  mutate(ucdp_vecinos = replace_na(ucdp_vecinos, 0))


# 7. Violencia político-criminal (Votar entre Balas)
# Cada fila es una víctima, así que contamos eventos únicos (id_evento)
# Sacamos las agresiones contra policías (área "Seguridad") en la medida principal

veb <- read_csv("data/votar_entre_balas/political_violence.csv", show_col_types = FALSE) |>
  mutate(fecha = as.Date(fecha),
         cve_ent = suppressWarnings(as.integer(cve_edoINEGI)),
         cve_mun = suppressWarnings(as.integer(cve_munINEGI))) |>
  filter(!is.na(fecha), !is.na(cve_mun)) |>   #quitamos filas sin fecha o sin municipio
  inner_join(ventanas, join_by(between(fecha, inicio, fin)))

contar_veb <- function(datos) {
  datos |>
    group_by(cve_ent, cve_mun, eleccion) |>
    summarise(n = n_distinct(id_evento), .groups = "drop")
}

violencia <- violencia |>
  left_join(rename(contar_veb(filter(veb, !grepl("^Seguridad", area_gob))), veb_eventos = n),
            by = c("cve_ent", "cve_mun", "eleccion")) |>
  left_join(rename(contar_veb(veb), veb_todos = n),   # robustez: con policías
            by = c("cve_ent", "cve_mun", "eleccion")) |>
  left_join(rename(contar_veb(filter(veb, grepl("^Elecciones", area_gob))), veb_campanas = n),  # robustez: candidaturas y campañas
            by = c("cve_ent", "cve_mun", "eleccion")) |>
  mutate(across(starts_with("veb_"), ~ replace_na(.x, 0)))


# 8. Unir el INE con las claves del INEGI
# El INE enumera los municipios distinto al INEGI en varios estados, así que
# los emparejamos por nombre dentro de cada estado: primero los nombres iguales
# y luego, entre los que quedan, el más parecido.

limpiar_nombre <- function(x) {
  x <- chartr("ÁÉÍÓÚÜÑáéíóúüñÃ",
              "AEIOUUNaeiouunA", enc2utf8(x))
  x <- gsub("[^A-Z ]", "", toupper(x))
  trimws(gsub("\\s+", " ", x))
}

catalogo$nombre_limpio <- limpiar_nombre(catalogo$nombre)
cruce <- list()

for (g in split(distinct(nec, anio, ID_ESTADO, ID_MUNICIPIO, MUNICIPIO),
                ~ anio + ID_ESTADO, drop = TRUE)) {
  inegi <- filter(catalogo, cve_ent == g$ID_ESTADO[1])
  g$nombre_limpio <- limpiar_nombre(g$MUNICIPIO)
  g$cve_mun <- NA_integer_

  #nombres iguales
  iguales <- match(g$nombre_limpio, inegi$nombre_limpio)
  g$cve_mun <- inegi$cve_mun[iguales]
  libres <- filter(inegi, !cve_mun %in% g$cve_mun)

  #un nombre contenido en el otro (ej. "ROSARIO" y "Valle del Rosario")
  for (i in which(is.na(g$cve_mun))) {
    j <- which(sapply(libres$nombre_limpio, function(x)
      grepl(g$nombre_limpio[i], x, fixed = TRUE) || grepl(x, g$nombre_limpio[i], fixed = TRUE)))
    if (length(j) == 1) {
      g$cve_mun[i] <- libres$cve_mun[j]
      libres <- libres[-j, ]
    }
  }

  #para los que faltan, el nombre más parecido (distancia de edición)
  while (any(is.na(g$cve_mun))) {
    faltan <- which(is.na(g$cve_mun))
    d <- adist(g$nombre_limpio[faltan], libres$nombre_limpio) /
      outer(nchar(g$nombre_limpio[faltan]), nchar(libres$nombre_limpio), pmax)
    k <- which(d == min(d), arr.ind = TRUE)[1, ]
    g$cve_mun[faltan[k[1]]] <- libres$cve_mun[k[2]]
    libres <- libres[-k[2], ]
  }
  cruce[[length(cruce) + 1]] <- g
}

cruce <- bind_rows(cruce) |> select(anio, ID_ESTADO, ID_MUNICIPIO, cve_mun)
write_csv(cruce, "data/procesados/cruce_ine_inegi.csv")


# 9. Base final

base <- nec |>
  left_join(cruce, by = c("anio", "ID_ESTADO", "ID_MUNICIPIO")) |>
  rename(cve_ent = ID_ESTADO) |>
  left_join(violencia, by = c("cve_ent", "cve_mun", "anio" = "eleccion")) |>
  left_join(select(catalogo, cve_ent, cve_mun, municipio = nombre, pob_2020),
            by = c("cve_ent", "cve_mun")) |>
  mutate(cvegeo = sprintf("%02d%03d", cve_ent, cve_mun),
         ucdp_log = log1p(ucdp_eventos),
         ucdp_hubo = as.integer(ucdp_eventos > 0),
         veb_log = log1p(veb_eventos),
         veb_hubo = as.integer(veb_eventos > 0),
         ucdp_vecinos_log = log1p(ucdp_vecinos),
         log_lista_nominal = log(lista_nominal)) |>
  select(anio, estado, cve_ent, cve_mun, cvegeo, municipio, nec, n_candidaturas,
         ucdp_eventos, ucdp_log, ucdp_hubo, veb_eventos, veb_log, veb_hubo,
         veb_todos, veb_campanas, ucdp_vecinos, ucdp_vecinos_log,
         lista_nominal, log_lista_nominal, pob_2020)

write_csv(base, "data/procesados/base_analisis.csv")

#revisión rápida
base |>
  group_by(anio) |>
  summarise(municipios = n(), nec_medio = round(mean(nec), 2),
            con_violencia_ucdp = sum(ucdp_hubo), con_violencia_veb = sum(veb_hubo))
