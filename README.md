
# Análisis de Calidad Urbana y Precios de Alquiler en Madrid

Este repositorio contiene el código desarrollado en R para el Trabajo de Fin de Máster (TFM) titulado **"Análisis del modelo de ciudad de 15 minutos y su relación con el precio del alquiler en Madrid"**. El objetivo es analizar cómo distintos indicadores de calidad urbana (índices QOLI) se relacionan con el precio medio del alquiler (€/m²) en celdas de 1 km² dentro del municipio de Madrid.

---

##  Estructura del repositorio

### 1. `codigo_cells_madrid.R`

> **Función**: Preprocesamiento geoespacial inicial. Este script carga la malla regular de 1 km² sobre el municipio de Madrid y obtiene los elementos espaciales desde OpenStreetMap y Eurostat. También realiza la intersección con los límites municipales y genera las variables base necesarias para calcular los subíndices QOLI.

- Crea la cuadrícula de análisis.
- Usa `osmdata` para obtener amenities como transporte, sanidad, deporte, naturaleza, etc.
- Calcula los conteos y normaliza por área o población si es necesario.

### 2. `Indice.R`

> **Función**: Generación de los subíndices QOLI (calidad urbana por categorías) y del índice general.

- Incluye categorías como: salud, educación, compras, transporte, restauración, deporte, naturaleza y otros.
- Normaliza los subíndices en escala ordinal (1–10) y calcula el índice general como suma o media ponderada.
- Devuelve una tabla final con los índices por celda (ID).

### 3. `Alquiler.R`

> **Función**: Tratamiento de los datos de alquiler procedentes del MITMA (Ministerio de Transportes, Movilidad y Agenda Urbana).

- Asigna los precios medios de alquiler por m² (`ALQM2`) desde sección censal a cada celda de 1 km² mediante intersección espacial.
- Calcula valores medios para cada celda, listos para ser integrados con los índices de calidad urbana.

### 4. `estadísticos_alquiler_indices.R`

> **Función**: Análisis estadístico y espacial final.

- Correlación de Pearson entre ALQM2 y cada subíndice QOLI.
- Estima modelo de regresión lineal múltiple (OLS).
- Verifica autocorrelación espacial de los residuos con el test de Moran’s I.
- Ajusta un modelo espacial SAR (`lagsarlm`) para controlar autocorrelación.

---

##  Requisitos

- R >= 4.2
- Paquetes principales: `sf`, `osmdata`, `spdep`, `tidyverse`, `ggplot2`, `viridis`, `spatialreg`, `dplyr`, `readxl`, `ggthemes`, `units`, `tmap`

Puedes instalar los paquetes necesarios con:

```r
install.packages(c("sf", "osmdata", "spdep", "tidyverse", "ggplot2", "viridis", "spatialreg", "readxl", "units", "tmap"))
```

---

##  Reproducción del análisis

1. Ejecutar `codigo_cells_madrid.R` para generar la malla y extraer los elementos urbanos de interés.
2. Ejecutar `Indice.R` para obtener los subíndices y el índice general QOLI.
3. Ejecutar `Alquiler.R` para asociar el precio medio del alquiler a cada celda.
4. Ejecutar `estadísticos_alquiler_indices.R` para realizar el análisis estadístico y generar los mapas finales.

---

##  Datos

- **MITMA (datos de alquiler)**: https://www.mitma.gob.es
- **Eurostat (grids 1 km²)**: https://ec.europa.eu/eurostat
- **OpenStreetMap (OSM)**: https://www.openstreetmap.org

---

##  Resultados principales

- El modelo OLS muestra una relación significativa entre el precio del alquiler y algunos subíndices (especialmente `education`, `sport`, `nature`, `transport`).
- Se detecta autocorrelación espacial en los residuos, lo que justifica el uso de un modelo espacial SAR.
- El modelo SAR mejora el ajuste, mostrando una fuerte dependencia espacial (`rho ≈ 0.96`), lo cual sugiere que los precios están altamente condicionados por los valores vecinos.

---

##  Licencia

Este proyecto es parte de un TFM del máster de Big Data, Data Science & Inteligencia Artificial. Su contenido puede utilizarse con fines académicos siempre que se cite adecuadamente.

