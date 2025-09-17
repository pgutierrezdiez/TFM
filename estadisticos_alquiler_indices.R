
library(dplyr)
library(sf)

alquiler_sf <- readRDS("C:/Users/34633/Desktop/TFM/Datos/alquiler_sf.rds")


# --- Recalcular ALQM2 por celda ---

library(sf)
library(dplyr)



# Ver CRS de ambos
st_crs(grid)
st_crs(alquiler_sf)

# Forzar a que ambos estén en el mismo (usamos el de grid como referencia)
alquiler_sf <- st_transform(alquiler_sf, st_crs(grid))

# --- 1. Desactivar geometría esférica para evitar errores
sf::sf_use_s2(FALSE)

# --- 2. Intersecar grid (celdas 1 km²) con secciones censales con alquiler
intersec <- st_intersection(grid, alquiler_sf)

# --- 3. Calcular precio medio del alquiler por celda
alquiler_por_celda <- intersec %>%
  group_by(GRD_ID) %>%
  summarise(ALQM2 = mean(ALQM2, na.rm = TRUE))

# --- 4. Preparar para el join (quitar geometría y asegurar mismo tipo de clave)
alquiler_por_celda <- alquiler_por_celda %>% st_drop_geometry()
alquiler_por_celda$GRD_ID <- as.character(alquiler_por_celda$GRD_ID)
grid$GRD_ID <- as.character(grid$GRD_ID)

# --- 5. Unir el alquiler a la cuadrícula
grid <- grid %>%
  left_join(alquiler_por_celda, by = "GRD_ID")

# --- 6. Verificar que ya tienes ALQM2 en grid
names(grid)

grid <- grid %>%
  mutate(ALQM2 = coalesce(ALQM2.x, ALQM2.y)) %>%
  select(-ALQM2.x, -ALQM2.y)



# Suponiendo que ya tienes grid y grid_qoli cargados

# Asegurar que la clave GRD_ID está en el mismo formato
grid$GRD_ID <- as.character(grid$GRD_ID)
grid_qoli$GRD_ID <- as.character(grid_qoli$GRD_ID)

# Unir
grid_qoli <- grid_qoli %>%
  left_join(grid %>% st_drop_geometry() %>% select(GRD_ID, ALQM2),
            by = "GRD_ID")

# Ahora prepara el dataset numérico
datos_modelo <- grid_qoli %>%
  st_drop_geometry() %>%
  select(GRD_ID, ALQM2, dining, transport, health, education,
         sport, shopping, nature, other, general)




#Correlaciones#####################################
names(intersec)
library(dplyr)      # para select, mutate, filter, etc.
library(sf)         # para st_drop_geometry




alquiler_por_celda <- intersec %>%
  st_drop_geometry() %>%
  group_by(GRD_ID) %>%
  summarise(ALQM2 = mean(ALQM2, na.rm = TRUE))


# unir a grid_qoli
grid_qoli <- grid_qoli %>%
  left_join(alquiler_por_celda, by = "GRD_ID")

names(grid_qoli)



library(dplyr)

library(dplyr)

# Crear columna única ALQM2 priorizando las no vacías
grid_qoli <- grid_qoli %>%
  mutate(ALQM2 = coalesce(ALQM2.x, ALQM2.y, ALQM2.x.x, ALQM2.y.y)) %>%
  select(-ALQM2.x, -ALQM2.y, -ALQM2.x.x, -ALQM2.y.y)


# Verifica
names(grid_qoli)


# --- 1. Crear dataset numérico ---
datos_modelo <- grid_qoli %>%
  st_drop_geometry() %>%
  select(GRD_ID, ALQM2, dining, transport, health, education,
         sport, shopping, nature, other, general)

# --- 2. Matriz de correlación ---
correlaciones <- datos_modelo %>%
  select(ALQM2, dining, transport, health, education,
         sport, shopping, nature, other, general) %>%
  cor(use = "complete.obs", method = "pearson")

# --- 3. Extraer correlación de ALQM2 con los índices ---
tabla_corr <- as.data.frame(correlaciones["ALQM2", ])
tabla_corr <- tibble::rownames_to_column(tabla_corr, "Indice_QOLI")
colnames(tabla_corr) <- c("Indice_QOLI", "Correlacion_con_ALQM2")

# --- 4. Ordenar de mayor a menor correlación ---
tabla_corr <- tabla_corr %>%
  filter(Indice_QOLI != "ALQM2") %>%   # quitamos la autocorrelación
  arrange(desc(Correlacion_con_ALQM2))

print(tabla_corr)











# --- 1. Crear dataset numérico con índices y alquiler ---
datos_modelo <- grid_qoli %>%
  st_drop_geometry() %>%
  select(GRD_ID, ALQM2, dining, transport, health, education,
         sport, shopping, nature, other, general)

# --- 2. Calcular correlaciones Pearson ---
correlaciones <- datos_modelo %>%
  select(ALQM2, dining, transport, health, education,
         sport, shopping, nature, other, general) %>%
  cor(use = "complete.obs", method = "pearson")

# --- 3. Convertir a tabla legible ---
tabla_corr <- as.data.frame(correlaciones["ALQM2", ])
tabla_corr <- tibble::rownames_to_column(tabla_corr, "Indice_QOLI")
colnames(tabla_corr) <- c("Indice_QOLI", "Correlacion_con_ALQM2")

# --- 4. Mostrar tabla ordenada ---
tabla_corr <- tabla_corr %>%
  arrange(desc(Correlacion_con_ALQM2))

print(tabla_corr)


# Regresión OLS Clasica

modelo_ols <- lm(ALQM2 ~ dining + transport + health + education +
                   sport + shopping + nature,
                 data = datos_modelo)

summary(modelo_ols)


#Diagnóstico de Multicolinialidad 

library(car)
vif(modelo_ols)

# Regresión con Errores Robustos

library(lmtest)
library(sandwich)

coeftest(modelo_ols, vcov = vcovHC(modelo_ols, type = "HC3"))


#Dependencia Espacial (Moran)

library(spdep)

# Crear vecinos (Queen)
# Filtrar solo las observaciones completas usadas en el modelo
grid_model <- grid %>% 
  filter(!is.na(ALQM2))

# Crear vecinos Queen sobre esas celdas
vecinos <- poly2nb(as_Spatial(grid_model))
lw <- nb2listw(vecinos, style = "W")

# Ajustar el modelo solo con las observaciones completas
datos_modelo <- grid_model %>%
  st_drop_geometry() %>%
  select(GRD_ID, ALQM2, starts_with("QOLI"))

modelo_ols <- lm(ALQM2 ~ ., data = datos_modelo %>% select(-GRD_ID))

# Residuos y Moran's I
residuos <- residuals(modelo_ols)
moran.test(residuos, lw)



#Modelo Espacial (SAR)

library(spatialreg)

modelo_sar <- lagsarlm(ALQM2 ~ ., data = datos_modelo %>% select(-GRD_ID), listw = lw)
summary(modelo_sar)
