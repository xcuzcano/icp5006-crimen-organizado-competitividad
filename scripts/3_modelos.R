# Script 3: modelos de regresión
#
# Modelos para las secciones "Construcción de los modelos" y "Resultados"
# Usa la base del script 1 (data/procesados/base_analisis.csv)
#
# Paquetes nuevos (x si no están):
# install.packages(c("sandwich", "lmtest", "marginaleffects", "flextable", "officer"))
#
# Guarda las tablas en la carpeta tablas/ (también un Word con las tablas listas)
# y la figura 4 en figuras/

library(dplyr)
library(tidyr)
library(readr)
library(ggplot2)
library(sandwich)
library(lmtest)
library(marginaleffects)
library(flextable)

dir.create("tablas", showWarnings = FALSE)
dir.create("figuras", showWarnings = FALSE)

#estado y año como factores para que entren como efectos fijos
base <- read_csv("data/procesados/base_analisis.csv", show_col_types = FALSE) |>
  mutate(estado = factor(estado),
         anio = factor(anio),
         veb_campanas_log = log1p(veb_campanas),
         veb_todos_log = log1p(veb_todos))

#cada municipio aparece hasta 3 veces (2018, 2021, 2024)entonces sus
#observaciones no son independientes, por tanto, agrupamos los errores por municipio
ee_cluster <- function(m) vcovCL(m, cluster = ~ cvegeo)


# 1. Modelos principales
# M1 solo la violencia, M2 agrega controles, M3 agrega VeB, M4 agrega vecinos (H2)

m1 <- lm(nec ~ ucdp_log, data = base)
m2 <- lm(nec ~ ucdp_log + log_lista_nominal + estado + anio, data = base)
m3 <- lm(nec ~ ucdp_log + veb_log + log_lista_nominal + estado + anio, data = base)
m4 <- lm(nec ~ ucdp_log + veb_log + ucdp_vecinos_log + log_lista_nominal + estado + anio, data = base)

#para mirar rápido los coeficientes con errores agrupados
coeftest(m4, vcov. = ee_cluster(m4))[2:5, ]


# 2. Robustez (mismos controles que M4)
# r1: violencia como 0/1 en vez de cantidad
# r2: VeB solo área "Elecciones y campañas" (candidaturas y equipos de campaña)
# r3: VeB incluyendo a policías
# y el M4 por separado para cada año

r1 <- lm(nec ~ ucdp_hubo + veb_hubo + ucdp_vecinos_log + log_lista_nominal + estado + anio, data = base)
r2 <- lm(nec ~ ucdp_log + veb_campanas_log + ucdp_vecinos_log + log_lista_nominal + estado + anio, data = base)
r3 <- lm(nec ~ ucdp_log + veb_todos_log + ucdp_vecinos_log + log_lista_nominal + estado + anio, data = base)

#en un solo año no va el efecto fijo de año
m_2018 <- lm(nec ~ ucdp_log + veb_log + ucdp_vecinos_log + log_lista_nominal + estado,
             data = filter(base, anio == "2018"))
m_2021 <- lm(nec ~ ucdp_log + veb_log + ucdp_vecinos_log + log_lista_nominal + estado,
             data = filter(base, anio == "2021"))
m_2024 <- lm(nec ~ ucdp_log + veb_log + ucdp_vecinos_log + log_lista_nominal + estado,
             data = filter(base, anio == "2024"))


# 3. Tablas de resultados
# cada celda: coeficiente con estrellas y el error estándar entre paréntesis

nombres <- c(ucdp_log          = "Violencia del crimen organizado (log)",
             ucdp_hubo         = "Hubo violencia del crimen organizado (0/1)",
             veb_log           = "Violencia político-criminal (log)",
             veb_campanas_log  = "Violencia político-criminal, solo campañas (log)",
             veb_todos_log     = "Violencia político-criminal, con policías (log)",
             veb_hubo          = "Hubo violencia político-criminal (0/1)",
             ucdp_vecinos_log  = "Violencia en municipios vecinos (log)",
             log_lista_nominal = "Tamaño del municipio (log lista nominal)",
             "(Intercept)"     = "Constante")

dec <- function(x, d = 3) formatC(x, format = "f", digits = d, decimal.mark = ",")

estrellas <- function(p) {
  case_when(p < 0.001 ~ "***", p < 0.01 ~ "**", p < 0.05 ~ "*", p < 0.1 ~ "+", TRUE ~ "")
}

columna <- function(m, fe_anio = "Sí") {
  ct <- coeftest(m, vcov. = ee_cluster(m))
  vars <- intersect(names(nombres), rownames(ct))
  celdas <- paste0(dec(ct[vars, 1]), estrellas(ct[vars, 4]), " (", dec(ct[vars, 2]), ")")
  r2_aj <- max(summary(m)$adj.r.squared, 0)   #en M1 sale levemente negativo, así no aparece "-0,00"
  c(setNames(celdas, nombres[vars]),
    "Efectos fijos de estado" = ifelse(length(coef(m)) > 2, "Sí", "No"),
    "Efectos fijos de año"    = fe_anio,
    "Observaciones"           = format(nobs(m), big.mark = ".", decimal.mark = ","),
    "R² ajustado"             = dec(r2_aj, 2))
}

juntar <- function(columnas) {
  filas <- c(nombres, "Efectos fijos de estado", "Efectos fijos de año", "Observaciones", "R² ajustado")
  filas <- filas[filas %in% unlist(lapply(columnas, names))]
  tabla <- tibble(Variable = unname(filas))
  for (n in names(columnas)) tabla[[n]] <- unname(columnas[[n]][filas])
  mutate(tabla, across(-Variable, ~ replace_na(.x, "")))
}

tabla_modelos <- juntar(list(M1 = columna(m1, fe_anio = "No"),
                             M2 = columna(m2),
                             M3 = columna(m3),
                             M4 = columna(m4)))

tabla_robustez <- juntar(list("Binarias"          = columna(r1),
                              "VeB solo campañas" = columna(r2),
                              "VeB con policías"  = columna(r3),
                              "Solo 2018"         = columna(m_2018, fe_anio = "Un solo año"),
                              "Solo 2021"         = columna(m_2021, fe_anio = "Un solo año"),
                              "Solo 2024"         = columna(m_2024, fe_anio = "Un solo año")))

write_csv(tabla_modelos, "tablas/tabla_modelos.csv")
write_csv(tabla_robustez, "tablas/tabla_robustez.csv")
print(tabla_modelos, width = Inf)
print(tabla_robustez, width = Inf)

#las mismas tablas en Word, para pegarlas directo en el informe
nota <- paste("Coeficientes de MCO; errores estándar agrupados por municipio entre paréntesis.",
              "+ p<0,1; * p<0,05; ** p<0,01; *** p<0,001.",
              "Variable dependiente: número efectivo de candidaturas.")

tabla_word <- function(tabla, titulo) {
  #el error estándar en otra línea para que la tabla quepa en la hoja
  tabla <- mutate(tabla, across(-Variable, ~ sub(" \\(", "\n(", .x)))
  flextable(tabla) |>
    set_caption(titulo) |>
    add_footer_lines(nota) |>
    font(fontname = "Times New Roman", part = "all") |>
    fontsize(size = 9, part = "all") |>
    bold(part = "header") |>
    align(j = -1, align = "center", part = "all") |>
    padding(padding.top = 1, padding.bottom = 1, part = "all") |>
    autofit() |>
    fit_to_width(max_width = 6.5)
}

save_as_docx("Modelos principales" = tabla_word(tabla_modelos, "Tabla de Violencia y competitividad electoral municipal (MCO)"),
             "Robustez" = tabla_word(tabla_robustez, "Tabla de Pruebas de robustez"),
             path = "tablas/tablas_modelos.docx")


# 4. Valores predichos con el M4
# cambiamos solo la cantidad de eventos y dejamos el resto de las variables
# como están en cada municipio... después se promedia

eventos <- c(0, 1, 2, 5, 10, 20, 50, 100)

pred_h1 <- avg_predictions(m4, variables = list(ucdp_log = log1p(eventos)), vcov = ~ cvegeo) |>
  as_tibble() |>
  transmute(tipo = "En el propio municipio (H1)", eventos = round(expm1(ucdp_log)),
            nec = estimate, ic_inf = conf.low, ic_sup = conf.high)

pred_h2 <- avg_predictions(m4, variables = list(ucdp_vecinos_log = log1p(eventos)), vcov = ~ cvegeo) |>
  as_tibble() |>
  transmute(tipo = "Promedio en municipios vecinos (H2)", eventos = round(expm1(ucdp_vecinos_log)),
            nec = estimate, ic_inf = conf.low, ic_sup = conf.high)
#el round() es necesario, si no expm1(log1p(10)) no da exactamente 10

predicciones <- bind_rows(pred_h1, pred_h2)
write_csv(mutate(predicciones, across(where(is.numeric), ~ round(.x, 3))), "tablas/predicciones.csv")
predicciones

#magnitud: cuánto cambia el NEC de 0 a 10 eventos y eso en desviaciones estándar
predicciones |>
  group_by(tipo) |>
  summarise(nec_0 = nec[eventos == 0], nec_10 = nec[eventos == 10]) |>
  mutate(diferencia = nec_10 - nec_0,
         en_desv_est = diferencia / sd(base$nec))


# 5. Figura 4: NEC predicho

azul <- "#2a78d6"
gris <- "#52514e"

fig4 <- ggplot(predicciones, aes(x = eventos, y = nec)) +
  geom_ribbon(aes(ymin = ic_inf, ymax = ic_sup), fill = azul, alpha = 0.18) +
  geom_line(color = azul, linewidth = 0.8) +
  geom_point(color = azul, size = 2.4) +
  facet_wrap(~ tipo) +
  scale_x_continuous(trans = "log1p", breaks = eventos) +
  scale_y_continuous(labels = scales::label_number(accuracy = 0.1, decimal.mark = ",")) +
  labs(title = "NEC predicho según la violencia del crimen organizado",
       subtitle = "Modelo M4; promedio de las predicciones con IC 95%, manteniendo el resto de las variables como se observan",
       x = "Eventos de violencia en los 6 meses previos (escala logarítmica)",
       y = "Número efectivo de candidaturas predicho",
       caption = "Fuente: elaboración propia con datos del INE (SICEE), UCDP GED 26.1, Votar entre Balas e INEGI.") +
  theme_minimal(base_size = 11) +
  theme(plot.title = element_text(face = "bold", size = 12),
        plot.subtitle = element_text(color = gris, size = 9),
        plot.caption = element_text(color = gris, hjust = 0, size = 8.5),
        axis.text = element_text(color = gris),
        panel.grid.minor = element_blank(),
        strip.text = element_text(face = "bold"))

ggsave("figuras/fig4_nec_predicho.png", fig4, width = 8, height = 4.5, dpi = 300, bg = "white")
