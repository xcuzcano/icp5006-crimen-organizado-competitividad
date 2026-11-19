# Script 2: análisis descriptivo
#
# Tablas y gráficos para la sección "Operacionalización y análisis descriptivo".
# Usa la base que armamos en el script 1 (data/procesados/base_analisis.csv)
#
# Guarda las tablas en la carpeta tablas/ y las figuras en figuras/

library(dplyr)
library(tidyr)
library(readr)
library(ggplot2)
library(sf)

dir.create("tablas", showWarnings = FALSE)
dir.create("figuras", showWarnings = FALSE)

base <- read_csv("data/procesados/base_analisis.csv", show_col_types = FALSE)

#colores y estilo que usamos en todos los gráficos
azul    <- "#2a78d6"
naranjo <- "#eb6834"
gris    <- "#52514e"

tema <- theme_minimal(base_size = 11) +
  theme(plot.title = element_text(face = "bold", size = 12),
        plot.subtitle = element_text(color = gris),
        plot.caption = element_text(color = gris, hjust = 0, size = 8.5),
        axis.text = element_text(color = gris),
        panel.grid.minor = element_blank(),
        strip.text = element_text(face = "bold"))

fuente <- "Fuente: elaboración propia con datos del INE (SICEE), UCDP GED 26.1 y Votar entre Balas."

#para mostrar los números con coma decimal
coma <- function(x, d = 2) format(round(x, d), nsmall = d, decimal.mark = ",", big.mark = ".")


# 1. Estadísticos descriptivos de cada variable

variables <- c(nec          = "Número efectivo de candidaturas (VD)",
               ucdp_eventos = "Eventos de violencia criminal, UCDP (VI)",
               veb_eventos  = "Eventos de violencia político-criminal, VeB",
               ucdp_vecinos = "Eventos UCDP promedio en municipios vecinos (H2)",
               lista_nominal = "Lista nominal (electores)")

#no incluimos la participación: en Jalisco y Chiapas 2018 los votos totales
#superan la lista nominal y además podría ser consecuencia de la violencia

tabla_desc <- base |>
  select(all_of(names(variables))) |>
  pivot_longer(everything(), names_to = "variable", values_to = "valor") |>
  group_by(variable) |>
  summarise(N = n(),
            Media = mean(valor),
            `Desv. estándar` = sd(valor),
            Mínimo = min(valor),
            Mediana = median(valor),
            Máximo = max(valor),
            `% de ceros` = mean(valor == 0) * 100) |>
  mutate(variable = factor(variable, levels = names(variables), labels = variables)) |>
  arrange(variable)

write_csv(tabla_desc, "tablas/tabla_descriptivos.csv")
tabla_desc


# 2. Resumen por elección

tabla_anio <- base |>
  group_by(Elección = anio) |>
  summarise(Municipios = n(),
            `NEC medio` = mean(nec),
            `NEC mediana` = median(nec),
            `% con violencia criminal (UCDP)` = mean(ucdp_hubo) * 100,
            `Eventos UCDP` = sum(ucdp_eventos),
            `% con violencia político-criminal (VeB)` = mean(veb_hubo) * 100,
            `Eventos VeB` = sum(veb_eventos))

write_csv(tabla_anio, "tablas/tabla_por_eleccion.csv")
tabla_anio


# 3. Figura 1: distribución del NEC por elección

medias <- base |> group_by(anio) |> summarise(media = mean(nec))

fig1 <- ggplot(base, aes(x = nec)) +
  geom_histogram(binwidth = 0.25, boundary = 1, fill = azul, color = "white") +
  geom_vline(data = medias, aes(xintercept = media), linetype = "dashed") +
  geom_text(data = medias, aes(x = media, y = Inf, label = paste("media", coma(media))),
            hjust = -0.1, vjust = 1.6, size = 3.2) +
  facet_wrap(~ anio, ncol = 1) +
  scale_x_continuous(breaks = 1:10) +
  labs(title = "Distribución del número efectivo de candidaturas",
       subtitle = "Elecciones de ayuntamiento, por año",
       x = "Número efectivo de candidaturas (NEC)", y = "Municipios",
       caption = fuente) +
  tema

ggsave("figuras/fig1_nec_distribucion.png", fig1, width = 7, height = 7, dpi = 300, bg = "white")


# 4. Figura 2: NEC medio según el nivel de violencia
# Agrupamos los municipios según cuántos eventos UCDP tuvieron en la ventana

por_violencia <- base |>
  mutate(nivel = cut(ucdp_eventos, c(-Inf, 0, 2, 9, Inf),
                     labels = c("0", "1–2", "3–9", "10 o más"))) |>
  group_by(nivel) |>
  summarise(municipios = n(),
            nec_medio = mean(nec),
            ic_inf = nec_medio - 1.96 * sd(nec) / sqrt(n()),
            ic_sup = nec_medio + 1.96 * sd(nec) / sqrt(n()),
            lista_nominal_mediana = median(lista_nominal))

write_csv(por_violencia, "tablas/tabla_nec_por_violencia.csv")
por_violencia
#aquí ojo: los municipios con más violencia son mucho más grandes (lista nominal)
#por eso en los modelos controlamos por tamaño

por_violencia <- por_violencia |>
  mutate(etiqueta = paste0(nivel, "\n(n = ", format(municipios, big.mark = ".", decimal.mark = ",", trim = TRUE), ")"),
         etiqueta = factor(etiqueta, levels = etiqueta))  #para que respete el orden 0, 1-2, 3-9, 10+

fig2 <- ggplot(por_violencia, aes(x = etiqueta, y = nec_medio)) +
  geom_errorbar(aes(ymin = ic_inf, ymax = ic_sup), width = 0.12, color = azul) +
  geom_point(size = 3.2, color = azul) +
  scale_y_continuous(breaks = seq(2.6, 4, by = 0.2), labels = function(x) coma(x, 1)) +
  labs(title = "NEC medio según la violencia criminal previa a la elección",
       subtitle = "Media e intervalo de confianza al 95%; sin controles",
       x = "Eventos de violencia (UCDP) en los 6 meses previos", y = "NEC medio",
       caption = paste0(fuente, "\nLos municipios con más violencia también son más grandes.")) +
  tema

ggsave("figuras/fig2_nec_por_violencia.png", fig2, width = 7, height = 4.5, dpi = 300, bg = "white")


# 5. Figura 3: mapa del NEC 2024 y eventos UCDP
# Pasamos el mapa a una proyección de México en metros (EPSG 6372) para poder
# simplificarlo (si no, pesa mucho para dibujarlo)

mapa <- st_read("data/inegi/municipios_inegi.gpkg", quiet = TRUE) |>
  transmute(cve_ent = as.integer(cve_ent), cve_mun = as.integer(cve_mun)) |>
  st_transform(6372) |>
  st_simplify(dTolerance = 300, preserveTopology = TRUE)

estados <- mapa |> group_by(cve_ent) |> summarise()

mapa_2024 <- mapa |>
  left_join(filter(base, anio == 2024) |> select(cve_ent, cve_mun, nec),
            by = c("cve_ent", "cve_mun"))

eventos_2024 <- read_csv("data/procesados/ucdp_eventos_municipio.csv", show_col_types = FALSE) |>
  filter(eleccion == 2024) |>
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326) |>
  st_transform(6372)

fig3 <- ggplot() +
  geom_sf(data = mapa_2024, aes(fill = nec), color = NA) +
  geom_sf(data = estados, fill = NA, color = "white", linewidth = 0.25) +
  geom_sf(data = eventos_2024, color = naranjo, size = 0.6, alpha = 0.6) +
  scale_fill_gradientn(colors = c("#cde2fb", "#6da7ec", "#256abf", "#0d366b"),
                       limits = c(1, 7), breaks = 1:7, oob = scales::squish,
                       na.value = "#f0efec", name = "NEC 2024") +
  labs(title = "Competitividad electoral 2024 y violencia criminal previa",
       subtitle = "Relleno: NEC por municipio. Gris: sin elección municipal en 2024 (Durango, Veracruz)\no por usos y costumbres. Puntos: eventos UCDP entre dic. 2023 y jun. 2024",
       caption = fuente) +
  tema +
  theme(axis.text = element_blank(), panel.grid = element_blank(),
        legend.position = c(0.88, 0.75))

ggsave("figuras/fig3_mapa_2024.png", fig3, width = 8, height = 5.5, dpi = 300, bg = "white")
