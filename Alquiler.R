rm(list=ls())

library(sf)
library(dplyr)
library(ggplot2)
library(ineAtlas)

# --- 1. Cargar celdas de 1 km² ---
grid <- st_read("C:/Users/34633/Desktop/TFM/Datos/raster_cells_madrid.gpkg")

# --- 2. Cargar datos de alquiler ---
alquiler <- readRDS("C:/Users/34633/Downloads/indices_alquiler_datos.rds")

alquiler %>% 
  filter(year %in% 2019:2021, 
         mun_id == "28079") %>% 
  group_by(CUSEC) %>% 
  reframe(ALQM2 = mean(ALQM2_LV_M_VC,na.rm=T)) %>% 
  select(CUSEC,ALQM2) %>% 
  unique() -> alquiler
  

# --- 3. Descargar geometría de secciones censales ---
data_sf <- get_tract_geom(year = 2021)

# --- 4. Filtrar solo el municipio de Madrid ---
tracts_madrid <- data_sf %>% 
  filter(municipality == "Madrid")

# --- 5. Hacer el join desde la geometría ---
# Mantenemos la geometría de tracts_madrid y unimos los datos de alquiler
alquiler_sf <- tracts_madrid %>%
  left_join(
    alquiler,
    by = c("tract_code" = "CUSEC")
  )

# Después de crear alquiler_sf la primera vez
saveRDS(alquiler_sf, "C:/Users/34633/Desktop/TFM/Datos/alquiler_sf.rds")


# --- 6. Asegurar que el CRS coincide con el de grid ---
if (st_crs(alquiler_sf) != st_crs(grid)) {
  alquiler_sf <- st_transform(alquiler_sf, st_crs(grid))
}


# --- 7. Intersecar celdas y secciones censales ---
sf::sf_use_s2(FALSE)
intersec <- st_intersection(grid, alquiler_sf)


# --- 8. Agregar: calcular precio medio por celda ---
alquiler_por_celda <- intersec %>%
  group_by(GRD_ID) %>%
  summarise(
    ALQM2 = mean(ALQM2, na.rm = TRUE)
  )

# --- 10. Unir con cuadrícula ---
alquiler_por_celda <- alquiler_por_celda %>% st_drop_geometry()
grid$GRD_ID <- as.character(grid$GRD_ID)
alquiler_por_celda$GRD_ID <- as.character(alquiler_por_celda$GRD_ID)
grid <- left_join(grid, alquiler_por_celda, by = "GRD_ID")


# --- 11. Interpolar para solucionar NAs

mapa_alquiler_distrito <- ggplot(alquiler_sf)+
  geom_sf(aes(fill = ALQM2),color = NA)+
  scale_fill_viridis_c(option = "plasma", na.value = "grey70", name = "€/m²")+
  labs(
    title = "Precio medio del alquiler por distrito censal",
    subtitle = "Municipio de Madrid – Año 2021",
    caption = "Fuente: MITMA, INE Atlas y elaboración propia"
  ) +
  theme_void()+
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5)
  ) +
  coord_sf(xlim = c(-3.9, -3.5), ylim = c(40.30, 40.65))

# Guardar en PNG en alta resolución
png("C:/Users/34633/Desktop/TFM/graficas/mapa_alquiler_distrito.png",
    width = 4000, height = 3000, res = 600)
print(mapa_alquiler_distrito)
dev.off()


# --- 11. Visualizar ---
mapa_alquiler <- ggplot(grid) +
  geom_sf(aes(fill = ALQM2), color = "grey30") +
  scale_fill_viridis_c(option = "plasma", na.value = "grey70", name = "€/m²") +
  labs(
    title = "Precio medio del alquiler por celda (1 km²)",
    subtitle = "Municipio de Madrid – Año 2021",
    caption = "Fuente: MITMA, INE Atlas y elaboración propia"
  ) +
  theme_void() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5)
  ) +
  coord_sf(xlim = c(-3.9, -3.5), ylim = c(40.30, 40.65))

print (mapa_alquiler)


# Guardar en PNG en alta resolución
png("C:/Users/34633/Desktop/TFM/graficas/mapa_alquiler.png",
    width = 4000, height = 3000, res = 600)
print(mapa_alquiler)
dev.off()



# Asegúrate de que GRD_ID es character en ambos
library(sf)
grid_qoli <- st_read("C:/Users/34633/Desktop/TFM/Datos/grid_qoli_madrid_final.gpkg")


grid_qoli$GRD_ID <- as.character(grid_qoli$GRD_ID)
grid$GRD_ID <- as.character(grid$GRD_ID)

# Unir ambos datasets por GRD_ID
grid_completo <- left_join(grid_qoli, grid %>% st_drop_geometry() %>% select(GRD_ID, ALQM2), by = "GRD_ID")

# --- Densidad ---
library(ggplot2)

# --- Mapa de densidad de población por celda (1 km²) ---
mapa_densidad <- ggplot(grid) +
  geom_sf(aes(fill = TOT_P_2021), color = NA) +
  scale_fill_viridis_c(
    option = "plasma",
    name = "Habitantes/km²",
    breaks = seq(0, max(grid$TOT_P_2021, na.rm = TRUE), by = 10000),
    labels = scales::comma_format()
  ) +
  labs(
    title = "Densidad de población por celda (1 km²)",
    subtitle = "Municipio de Madrid – Año 2021",
    caption = "Fuente: Eurostat (grid 1 km²) e INE Atlas"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5)
  ) +
  coord_sf(xlim = c(-3.9, -3.5), ylim = c(40.30, 40.65))

print(mapa_densidad)

# Guardar en alta resolución
png("C:/Users/34633/Desktop/TFM/graficas/mapa_densidad.png",
    width = 4000, height = 3000, res = 600)
print(mapa_densidad)
dev.off()
