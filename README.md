# Crimen organizado y competitividad electoral municipal en México

Proyecto final del curso **Medición y Análisis Dimensional de Datos Políticos (ICP5006)**

## Pregunta de investigación

¿En qué medida la violencia asociada al crimen organizado se relaciona con una menor competitividad electoral a nivel municipal?

- **H1:** una mayor intensidad de violencia asociada al crimen organizado se relaciona con una menor competitividad electoral municipal.
- **H2:** una mayor violencia en los municipios vecinos se relaciona con una menor competitividad electoral en el municipio, aun considerando la violencia dentro de sus límites.

La unidad de análisis es el **municipio-elección** (elecciones de ayuntamiento de 2018, 2021 y 2024; 5321 observaciones). La variable dependiente es el **número efectivo de candidaturas (NEC)**. La violencia se mide como el número de eventos en los seis meses previos a cada elección.

## Cómo reproducir el análisis

1. Abrir `trabajo_final.Rproj` en RStudio 
2. Instalar los paquetes (una sola vez):
   ```r
   install.packages(c("dplyr", "tidyr", "readr", "purrr", "ggplot2", "scales",
                      "sf", "spdep", "sandwich", "lmtest", "marginaleffects",
                      "flextable", "officer"))
   ```
3. Correr los tres scripts en orden:
   ```r
   source("scripts/1_limpieza_y_variables.R")
   source("scripts/2_descriptivos.R")
   source("scripts/3_modelos.R")
   ```

La primera vez, el script 1 descarga el mapa municipal del INEGI (requiere internet y tarda unos minutos)

## Scripts

| Script | Qué hace | Resultado principal |
|---|---|---|
| `1_limpieza_y_variables.R` | Calcula el NEC con los resultados del INE, asigna los eventos de UCDP a cada municipio por coordenadas, calcula la violencia en municipios vecinos, cuenta los eventos de Votar entre Balas, cruza las claves del INE con las del INEGI y arma la base final | `data/procesados/base_analisis.csv` |
| `2_descriptivos.R` | Tablas y figuras descriptivas | `tablas/tabla_descriptivos.csv`, `figuras/fig1` a `fig3` |
| `3_modelos.R` | Modelos de regresión (MCO con efectos fijos de estado y año, errores agrupados por municipio), pruebas de robustez y valores predichos | `tablas/tablas_modelos.docx`, `figuras/fig4_nec_predicho.png` |

## Organización de los archivos

```
trabajo_final/
├── trabajo_final.Rproj
├── README.md
├── scripts/                     scripts del 1 al 3
├── data/
│   ├── ine/                     votos por municipio y candidatura (ine_muncand_largo.csv)
│   ├── ucdp/                    eventos de UCDP filtrados a México y codebook
│   ├── votar_entre_balas/       base de violencia político-criminal
│   └── procesados/              bases intermedias y base final del análisis
├── tablas/                      tablas en CSV y Word
└── figuras/                     figuras en PNG
```

## Fuentes de datos

- **INE – Sistema de Consulta de la Estadística de las Elecciones (SICEE):** resultados de las elecciones de ayuntamiento 2018, 2021 y 2024. https://sicee.ine.mx/home
- **UCDP Georeferenced Event Dataset (GED) Global, versión 26.1** (Davies, Pettersson y Öberg, 2026; Sundberg y Melander, 2013): https://ucdp.uu.se/downloads/
- **Votar entre Balas** (Data Cívica, México Evalúa y Animal Político): https://votar-entre-balas.datacivica.org/datos-votar-entre-balas
- **INEGI – Marco Geoestadístico** (límites municipales y población 2020), descargado desde el servicio web del INEGI por el script 1: https://www.inegi.org.mx/temas/mg/

### Archivos que no están en el repositorio (ya que superan los 50 MB)

- **Zips del INE (aprox. 430 MB):** no hacen falta para reproducir el análisis porque el repositorio ya incluye los votos extraídos (`data/ine/ine_muncand_largo.csv`). Para extraerlos de nuevo: en https://sicee.ine.mx/home → ícono de base de datos → Ámbito "Local" → elegir el año (2018, 2021 y 2024) → descargar todos los archivos `*_PEL_<año>.zip`, sin descomprimir, en `data/ine/2018/`, `data/ine/2021/` y `data/ine/2024/`. Luego borrar `data/ine/ine_muncand_largo.csv` y correr el script 1, que lo vuelve a construir desde los zips.
- **Base global de UCDP GED 26.1 (aprox. 38 MB comprimida):** en https://ucdp.uu.se/downloads/ → "UCDP Georeferenced Event Dataset (GED) Global version 26.1" en CSV. Este repositorio tiene ya la versión filtrada a México (`data/ucdp/ucdp_ged_mexico.csv`), obtenida con `subset(ged, country == "Mexico")`.
- **Mapa municipal del INEGI (63 MB):** se descarga automáticamente al correr el script 1 y se guarda en `data/inegi/`.
