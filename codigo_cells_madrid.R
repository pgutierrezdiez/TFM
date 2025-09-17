library(sf)
library(tidyverse)

setwd("C:/Users/34633/Downloads")

raster <- st_read("grid_1km_surf.gpkg")


### Filtrar celdas en Madrid provincia

raster %>% 
  filter(NUTS2021_0 == "ES") -> raster_madrid

raster_madrid %>%
  filter(NUTS2021_3 == "ES300") -> raster_madrid

rm(raster)
gc()


### Filtrar municipio Madrid

setwd("C:/Users/34633/Desktop/TFM/lineas_limite/SHP_ETRS89/recintos_municipales_inspire_peninbal_etrs89")

municipios_geo <- st_read("recintos_municipales_inspire_peninbal_etrs89.shp")

municipios_geo %>% 
  mutate(mun_id = substr(NATCODE,7,11)) %>% 
  filter(mun_id == "28079") %>% 
  select(mun_id,geometry) -> madrid_mun



### Intersecar madrid municipio con celdas


st_crs(madrid_mun) == st_crs(raster_madrid)

crs_true <- st_crs(raster_madrid)

madrid_mun <- st_transform(madrid_mun$geometry, crs = crs_true)

intersection <- st_intersects(raster_madrid, madrid_mun)
intersection <- lengths(intersection) > 0 

intersection_true <- which(intersection == T)

raster_madrid_mun <- raster_madrid[intersection_true,]



### Adaptar proyeccion a WGS84

raster_madrid_mun <- st_transform(raster_madrid_mun, 4326)

### Guardar resultado celdas

setwd("C:/Users/34633/Desktop/TFM/Datos")

st_write(raster_madrid_mun, "raster_cells_madrid.gpkg")



### Población
library(sf)
library(tidyverse)
library(osmdata)
library(viridis)

library(ggplot2)
library(sf)


# Mapa Madrid celdas

# Pasamos el municipio a un borde único
mun_borde <- st_union(madrid_mun)

# Gráfico único: municipio + celdas
mapa_celdas <- ggplot() +
  geom_sf(data = raster_madrid_mun, aes(geometry = geom),
          fill = NA, color = "grey40", size = 0.2) +  # celdas
  geom_sf(data = mun_borde, fill = NA, color = "black", size = 0.6) +  # borde municipio
  coord_sf(datum = NA) +
  theme_void() +
  labs(title = "Municipio de Madrid dividido en celdas de 1 km²")

# Mostrar en pantalla
print(mapa_celdas)

# Guardar en PNG en alta resolución
png("C:/Users/34633/Desktop/TFM/graficas/madrid_celdas.png",
    width = 4000, height = 3000, res = 600)
print(mapa_celdas)
dev.off()



# --- Mapa de densidad de población (Madrid) ---
map <- ggplot(raster_madrid_mun) +
  geom_sf(aes(fill = TOT_P_2021), color = NA) +   # sin bordes
  scale_fill_viridis_c(
    name = "Población",
    option = "magma",          # mismo estilo cromático
    breaks = c(2500, 5000, 7500, 15000),
    labels = c("2.500", "5.000", "7.500", "15.000"),
    limits = c(0, 20000),      # rango fijo
    oob = scales::squish
  ) +
  labs(
    title = "Densidad de población en Madrid (Municipio)",
    caption = "Fuente: Eurostat / INE / Elaboración propia"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    legend.position = "right"
  )


# Muestra el mapa
print(map)

setwd("C:/Users/34633/Desktop/TFM/graficas")

png("mapa_madrid.png", width = 4000, height = 3000,res = 600)  
plot(map)
dev.off()




### Analisis Celda


library(sf)
library(dplyr)
library(osmdata)
library(openxlsx)

# Leer celdas con transporte/población, etc.
grid <- st_read("C:/Users/34633/Desktop/TFM/Datos/raster_cells_madrid.gpkg")

# Asegúrate de que cada celda tiene un identificador único (ajústalo si se llama diferente)
if (!"ID_CELL" %in% colnames(grid)) {
  grid$ID_CELL <- 1:nrow(grid)
}

### Definimos las categorías

categorias <- list(
  Comida_Primario = list(key = "amenity", value = "restaurant"),
  Comida_Secundario = list(key = "amenity", value = "fast_food"),
  Comida_Terciario = list(key = "amenity", value = "ice_cream"),
  
  Cafeteria_Secundario = list(key = "amenity", value = "cafe"),
  Cafeteria_Terciario1 = list(key = "amenity", value = "bar"),
  Cafeteria_Terciario2 = list(key = "amenity", value = "pub"),
  Cafeteria_Adicional = list(key = "amenity", value = "biergarten"),
  
  Transporte_Primario = list(key = "railway", value = "subway_entrance"),
  Transporte_Secundario = list(key = "public_transport", value = "platform"),
  Transporte_Terciario = list(key = "amenity", value = "parking"),
  Transporte_Adicional1 = list(key = "amenity", value = "bicycle_rental"),
  Transporte_Adicional2 = list(key = "amenity", value = "car_rental"),
  Transporte_Adicional3 = list(key = "amenity", value = "taxi"),
  
  Sanidad_Primario = list(key = "amenity", value = "pharmacy"),
  Sanidad_Secundario1 = list(key = "amenity", value = "doctors"),
  Sanidad_Terciario1 = list(key = "amenity", value = "clinic"),
  Sanidad_Adicional1 = list(key = "amenity", value = "dentist"),
  Sanidad_Adicional2 = list(key = "amenity", value = "hospital"),
  Sanidad_Adicional3 = list(key = "amenity", value = "veterinary"),
  Sanidad_Adicional4 = list(key = "healthcare", value = "psychotherapist"),
  
  Educacion_Primario = list(key = "amenity", value = "school"),
  Educacion_Secundario = list(key = "amenity", value = "kindergarten"),
  Educacion_Terciario = list(key = "amenity", value = "library"),
  Educacion_Adicional1 = list(key = "amenity", value = "university"),
  Educacion_Adicional2 = list(key = "amenity", value = "driving_school"),
  Educacion_Adicional3 = list(key = "amenity", value = "language_school"),
  Educacion_Adicional4 = list(key = "amenity", value = "music_school"),
  
  Deporte_Primario = list(key = "leisure", value = "pitch"), # genérico, pero podría filtrar más
  Deporte_Secundario = list(key = "leisure", value = "fitness_centre"),
  Deporte_Terciario = list(key = "sport", value = "basketball"),
  Deporte_Adicional1 = list(key = "sport", value = "badminton"),
  Deporte_Adicional2 = list(key = "sport", value = "judo"),
  Deporte_Adicional3 = list(key = "sport", value = "tennis"),
  
  Compras_Primario = list(key = "shop", value = "supermarket"),
  Compras_Secundario = list(key = "shop", value = "health_food"),
  Compras_Terciario = list(key = "shop", value = "bakery"),
  Compras_Adicional1 = list(key = "shop", value = "shoes"),
  Compras_Adicional2 = list(key = "shop", value = "clothes"),
  Compras_Adicional3 = list(key = "shop", value = "greengrocer"),
  
  Naturaleza_Primario = list(key = "leisure", value = "park"),
  Naturaleza_Secundario = list(key = "natural", value = "tree"),  # NDVI no está en OSM
  Naturaleza_Terciario1 = list(key = "landuse", value = "meadow"),
  Naturaleza_Terciario2 = list(key = "natural", value = "water"),
  Naturaleza_Adicional1 = list(key = "natural", value = "hill"),
  Naturaleza_Adicional2 = list(key = "natural", value = "stone"),
  Naturaleza_Adicional3 = list(key = "landuse", value = "flowerbed")
)

resultados <- list()

for (i in 1:nrow(grid)) {
  cat("Procesando celda", i, "de", nrow(grid), "\n")
  celda <- grid[i, ]
  id <- celda$ID_CELL
  bb <- st_bbox(celda)
  
  fila <- list(ID_CELL = id)
  
  # Recorrer categorías OSM
  for (nombre in names(categorias)) {
    key <- categorias[[nombre]]$key
    value <- categorias[[nombre]]$value
    
    # Consulta OSM para esa celda y categoría
    query <- tryCatch({
      opq(bbox = bb) %>%
        add_osm_feature(key = key, value = value)
    }, error = function(e) return(NULL))
    
    if (is.null(query)) {
      fila[[nombre]] <- NA
      next
    }
    
    datos <- tryCatch({
      osmdata_sf(query)
    }, error = function(e) return(NULL))
    
    if (is.null(datos)) {
      fila[[nombre]] <- NA
      next
    }
    
    puntos <- datos$osm_points
    polys <- datos$osm_polygons
    
    total <- 0
    if (!is.null(puntos) && nrow(puntos) > 0) {
      puntos <- st_transform(puntos, st_crs(celda))
      puntos <- puntos[st_within(puntos, celda, sparse = FALSE), ]
      total <- total + nrow(puntos)
    }
    
    if (!is.null(polys) && nrow(polys) > 0) {
      polys <- st_transform(polys, st_crs(celda))
      intersec <- st_intersection(polys, celda)
      total <- total + nrow(intersec)
    }
    
    fila[[nombre]] <- total
  }
  
  resultados[[i]] <- fila
}



library(openxlsx)

setwd("C:/Users/34633/Desktop/TFM/Datos")

saveRDS(resultados, file = "resultados.rds")

### Mapas
names(resultados)
+++++++++++++++++++++++++++++++
library(ggplot2)


for(i in 1:length(resultados)){
  
  loop_i <- resultados[[i]]
  
  for(j in 1:length(loop_i)){
    
    loop_j <- loop_i[[j]]
    name_j <- names(loop_i)[j]
    
    if(j==1){
      next
    }else{
      data_loop <- data.frame(i,name_j,loop_j)
      
    }
    
    if(i==1 & j == 2){
      data_limpia <- data_loop
    }else{
      if(j == 1){
        next
      }else{
        data_limpia <- rbind.data.frame(data_limpia,data_loop)
      }
    }
    
  }
  
  print(i)
  
}

setwd("C:/Users/34633/Desktop/TFM/Datos")

saveRDS(resultados, file = "resultados2.rds")

names(data_limpia) <- c("cell_id","item","count")

705*46





### ÍNDICES Y REPRESENTACIÓN DE ESTOS

setwd("C:/Users/34633/Desktop/TFM/Datos")

data <- readRDS("resultados2.rds")


### ÍNDICES Y REPRESENTACIÓN DE ESTOS
suppressPackageStartupMessages({
  library(sf)
  library(dplyr)
  library(tidyr)
  library(purrr)
  library(spdep)
  library(ggplot2)
  library(viridis)
})


if (!"ID_CELL" %in% names(grid)) grid$ID_CELL <- seq_len(nrow(grid))

# --- 1) Ensamblar 'resultados' (lista) 

df_resultados <- resultados %>%
  purrr::map_dfr(~as.data.frame(.x)) %>%               
  mutate(ID_CELL = as.integer(ID_CELL)) %>%
  group_by(ID_CELL) %>%                                  
  summarise(across(everything(), ~ suppressWarnings(sum(as.numeric(.), na.rm = TRUE))))


grid <- grid %>% left_join(df_resultados, by = "ID_CELL")

# Rellenar NAs en amenidades a 0
amenity_cols <- setdiff(names(df_resultados), "ID_CELL")
if (length(amenity_cols)) {
  grid <- grid %>% mutate(across(all_of(amenity_cols), \(x) replace_na(x, 0)))
}

# --- 2) Vecindarios
grid_sp <- as(grid, "Spatial")
Neighbours <- poly2nb(grid_sp)

# --- 3) Pesos y función del índice (adaptada a tus nombres)
W1   <- 9
W2   <- 0.3
W3   <- 0.2
Wadd <- 0.1
Wadd_max <- 0.3
Wnb  <- 0.5
POP_VAR <- "TOT_P_2021"

index_qol <- function(dataset_sf,
                      pop_var = "TOT_P_2021",
                      primary,
                      secondary,
                      tertiary,     
                      additional,    
                      PerCapita = TRUE,
                      W1 = 9, W2 = 0.3, W3 = 0.2, Wadd = 0.1, Wadd_max = 0.3, Wnb = 0.5,
                      Neighbours) {
  dt <- st_drop_geometry(dataset_sf)
  
  # Asegurar columnas (si falta alguna, crear a 0)
  req_cols <- c(pop_var, primary, secondary, tertiary, additional)
  faltan <- setdiff(req_cols, names(dt))
  if (length(faltan)) dt[, faltan] <- 0
  
  RES <- as.numeric(dt[[pop_var]])
  
  # PRIMARY
  if (PerCapita) {
    primary_pc <- ifelse(RES > 0, dt[[primary]] / RES, dt[[primary]])
    neigh_sum_primary_pc <- sapply(seq_len(nrow(dt)), function(i) sum(primary_pc[Neighbours[[i]]], na.rm = TRUE))
    primary_corrected <- primary_pc + Wnb * neigh_sum_primary_pc
  } else {
    neigh_sum_primary <- sapply(seq_len(nrow(dt)), function(i) sum(dt[[primary]][Neighbours[[i]]], na.rm = TRUE))
    primary_corrected <- dt[[primary]] + Wnb * neigh_sum_primary
  }
  
  thr <- as.numeric(quantile(primary_corrected, 0.75, na.rm = TRUE))
  if (is.na(thr) || thr == 0) thr <- max(primary_corrected, na.rm = TRUE)
  ind_primary <- ifelse(primary_corrected > thr, W1, ceiling(primary_corrected / thr * W1))
  ind_primary[is.na(ind_primary)] <- 0
  
  # SECONDARY (presencia celda / vecinos)
  sec_here  <- dt[[secondary]] > 0
  sec_neigh <- sapply(seq_len(nrow(dt)), function(i) any(dt[[secondary]][Neighbours[[i]]] > 0, na.rm = TRUE))
  ind_secondary <- ifelse(sec_here, W2, ifelse(sec_neigh, W2 * Wnb, 0))
  
  # TERTIARY (dos variables)
  t1 <- tertiary[1]; t2 <- tertiary[2]
  if (!t1 %in% names(dt)) dt[[t1]] <- 0
  if (!t2 %in% names(dt)) dt[[t2]] <- 0
  
  t1_here  <- dt[[t1]] > 0
  t1_neigh <- sapply(seq_len(nrow(dt)), function(i) any(dt[[t1]][Neighbours[[i]]] > 0, na.rm = TRUE))
  ind_t1 <- ifelse(t1_here, W3, ifelse(t1_neigh, W3 * Wnb, 0))
  
  t2_here  <- dt[[t2]] > 0
  t2_neigh <- sapply(seq_len(nrow(dt)), function(i) any(dt[[t2]][Neighbours[[i]]] > 0, na.rm = TRUE))
  ind_t2 <- ifelse(t2_here, W3, ifelse(t2_neigh, W3 * Wnb, 0))
  
  # ADDITIONAL (suma celda + presencia vecinos, acotado)
  add_sum <- if (length(additional)) rowSums(dt[, additional, drop = FALSE], na.rm = TRUE) else rep(0, nrow(dt))
  add_neigh_presence <- if (length(additional)) {
    sapply(seq_len(nrow(dt)), function(i) any(colSums(dt[Neighbours[[i]], additional, drop = FALSE] > 0, na.rm = TRUE) > 0))
  } else rep(FALSE, nrow(dt))
  
  ind_add <- pmin(add_sum * Wadd + (add_neigh_presence * 1) * Wadd * Wnb, Wadd_max)
  ind_add[is.na(ind_add)] <- 0
  
  # SUB-ÍNDICE
  as.numeric(ind_primary + ind_secondary + ind_t1 + ind_t2 + ind_add)
}

# --- 4) Categorías usando TUS nombres de columnas
# COMIDA
Primary_dining     <- "Comida_Primario"         # restaurants
Secondary_dining   <- "Comida_Secundario"       # fast_food
Tertiary_dining    <- c("Comida_Terciario",     # ice_cream
                        "Cafeteria_Secundario") # cafe
Additional_dining  <- c("Cafeteria_Terciario1","Cafeteria_Terciario2","Cafeteria_Adicional")
Additional_dining  <- intersect(Additional_dining, names(grid))

# TRANSPORTE
Primary_transport    <- "Transporte_Primario"      # subway_entrance (si luego tienes departures, cámbialo aquí)
Secondary_transport  <- "Transporte_Secundario"    # platform
Tertiary_transport   <- c("Transporte_Terciario",  # parking
                          "Transporte_Adicional1") # bicycle_rental
Additional_transport <- c("Transporte_Adicional2","Transporte_Adicional3")
Additional_transport <- intersect(Additional_transport, names(grid))

# SANIDAD
Primary_health    <- "Sanidad_Primario"      # pharmacy
Secondary_health  <- "Sanidad_Secundario1"   # doctors
Tertiary_health   <- c("Sanidad_Terciario1", # clinic
                       "Sanidad_Adicional1") # dentist
Additional_health <- c("Sanidad_Adicional2","Sanidad_Adicional3","Sanidad_Adicional4")
Additional_health <- intersect(Additional_health, names(grid))

# EDUCACIÓN
Primary_edu    <- "Educacion_Primario"       # school
Secondary_edu  <- "Educacion_Secundario"     # kindergarten
Tertiary_edu   <- c("Educacion_Terciario",   # library
                    "Educacion_Adicional1")  # university
Additional_edu <- c("Educacion_Adicional2","Educacion_Adicional3","Educacion_Adicional4")
Additional_edu <- intersect(Additional_edu, names(grid))

# DEPORTE
Primary_sport    <- "Deporte_Primario"         # pitch
Secondary_sport  <- "Deporte_Secundario"       # fitness_centre
Tertiary_sport   <- c("Deporte_Terciario",     # basketball
                      "Deporte_Adicional3")    # tennis
Additional_sport <- c("Deporte_Adicional1","Deporte_Adicional2")
Additional_sport <- intersect(Additional_sport, names(grid))

# COMPRAS
Primary_shop    <- "Compras_Primario"        # supermarket
Secondary_shop  <- "Compras_Secundario"      # health_food
Tertiary_shop   <- c("Compras_Terciario",    # bakery
                     "Compras_Adicional1")   # shoes
Additional_shop <- c("Compras_Adicional2","Compras_Adicional3")
Additional_shop <- intersect(Additional_shop, names(grid))

# NATURALEZA
Primary_nature    <- "Naturaleza_Primario"       # park
Secondary_nature  <- "Naturaleza_Secundario"     # tree
Tertiary_nature   <- c("Naturaleza_Terciario1",  # meadow
                       "Naturaleza_Terciario2")  # water
Additional_nature <- c("Naturaleza_Adicional1","Naturaleza_Adicional2","Naturaleza_Adicional3")
Additional_nature <- intersect(Additional_nature, names(grid))

# --- 5) Calcular subíndices y general (per cápita)
Index <- data.frame(ID_CELL = grid$ID_CELL)

Index$dining <- index_qol(grid, POP_VAR, Primary_dining, Secondary_dining, Tertiary_dining, Additional_dining,
                          PerCapita = TRUE, W1=W1, W2=W2, W3=W3, Wadd=Wadd, Wadd_max=Wadd_max, Wnb=Wnb, Neighbours=Neighbours)

Index$transport <- index_qol(grid, POP_VAR, Primary_transport, Secondary_transport, Tertiary_transport, Additional_transport,
                             PerCapita = TRUE, W1=W1, W2=W2, W3=W3, Wadd=Wadd, Wadd_max=Wadd_max, Wnb=Wnb, Neighbours=Neighbours)

Index$health <- index_qol(grid, POP_VAR, Primary_health, Secondary_health, Tertiary_health, Additional_health,
                          PerCapita = TRUE, W1=W1, W2=W2, W3=W3, Wadd=Wadd, Wadd_max=Wadd_max, Wnb=Wnb, Neighbours=Neighbours)

Index$education <- index_qol(grid, POP_VAR, Primary_edu, Secondary_edu, Tertiary_edu, Additional_edu,
                             PerCapita = TRUE, W1=W1, W2=W2, W3=W3, Wadd=Wadd, Wadd_max=Wadd_max, Wnb=Wnb, Neighbours=Neighbours)

Index$sport <- index_qol(grid, POP_VAR, Primary_sport, Secondary_sport, Tertiary_sport, Additional_sport,
                         PerCapita = TRUE, W1=W1, W2=W2, W3=W3, Wadd=Wadd, Wadd_max=Wadd_max, Wnb=Wnb, Neighbours=Neighbours)

Index$shopping <- index_qol(grid, POP_VAR, Primary_shop, Secondary_shop, Tertiary_shop, Additional_shop,
                            PerCapita = TRUE, W1=W1, W2=W2, W3=W3, Wadd=Wadd, Wadd_max=Wadd_max, Wnb=Wnb, Neighbours=Neighbours)

Index$nature <- index_qol(grid, POP_VAR, Primary_nature, Secondary_nature, Tertiary_nature, Additional_nature,
                          PerCapita = TRUE, W1=W1, W2=W2, W3=W3, Wadd=Wadd, Wadd_max=Wadd_max, Wnb=Wnb, Neighbours=Neighbours)

# “Otros” (opcional): todo lo que no está en las categorías anteriores
ya_usadas <- c(Primary_dining, Secondary_dining, Tertiary_dining, Additional_dining,
               Primary_transport, Secondary_transport, Tertiary_transport, Additional_transport,
               Primary_health, Secondary_health, Tertiary_health, Additional_health,
               Primary_edu, Secondary_edu, Tertiary_edu, Additional_edu,
               Primary_sport, Secondary_sport, Tertiary_sport, Additional_sport,
               Primary_shop, Secondary_shop, Tertiary_shop, Additional_shop,
               Primary_nature, Secondary_nature, Tertiary_nature, Additional_nature)
otros_cols <- setdiff(amenity_cols, ya_usadas)
otros_cols <- intersect(otros_cols, names(grid))

if (length(otros_cols)) {
  sums_other <- rowSums(st_drop_geometry(grid)[, otros_cols, drop = FALSE], na.rm = TRUE)
  Index$other <- ifelse(max(sums_other) > 0, (sums_other / max(sums_other)) * 10, 0)
} else {
  Index$other <- 0
}

Index$general <- rowMeans(Index[, setdiff(names(Index), "ID_CELL")], na.rm = TRUE)

# Unir al grid y guardar
grid_qoli <- grid %>% left_join(Index, by = "ID_CELL")
st_write(grid_qoli, "C:/Users/34633/Desktop/TFM/Datos/grid_qoli_madrid_final.gpkg", delete_dsn = TRUE)

# --- 6) Mapas
plot_map <- function(sfobj, var, title){
  ggplot(sfobj) +
    geom_sf(aes_string(fill = var), color = NA) +
    scale_fill_viridis_c(guide = guide_legend(title = "Index value"),
                         limits = c(0,10), breaks = 0:10) +
    labs(title = title) +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5))
}



p_general   <- plot_map(grid_qoli, "general",   "QOLI – Índice General")
p_dining    <- plot_map(grid_qoli, "dining",    "Dining Index")
p_transport <- plot_map(grid_qoli, "transport", "Transport Index")
p_health    <- plot_map(grid_qoli, "health",    "Healthcare Index")
p_shopping  <- plot_map(grid_qoli, "shopping",  "Shopping Index")
p_sport     <- plot_map(grid_qoli, "sport",     "Sport Index")
p_edu       <- plot_map(grid_qoli, "education", "Education Index")
p_nature    <- plot_map(grid_qoli, "nature",    "Nature Index")
p_other     <- plot_map(grid_qoli, "other",     "Others Index")

# Muestra el general (puedes imprimir el resto igual)
print(p_general)

