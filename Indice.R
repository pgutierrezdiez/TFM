rm(list=ls())

library(sf)
library(spdep)
library(tidyverse)
library(viridis)


### CARGAR DATOS

setwd("C:/Users/34633/Desktop/TFM/Datos")

resultados <- readRDS("resultados.rds")
grid <- st_read("C:/Users/34633/Desktop/TFM/Datos/raster_cells_madrid.gpkg")



### ÍNDICES Y REPRESENTACIÓN DE ESTOS

if (!"ID_CELL" %in% names(grid)) grid$ID_CELL <- seq_len(nrow(grid))

# --- 1) Ensamblar 'resultados' (lista) 

df_resultados <- resultados %>%
  purrr::map_dfr(~as.data.frame(.x)) %>%               
  mutate(ID_CELL = as.integer(ID_CELL)) %>%
  group_by(ID_CELL) %>%                                  
  summarise(across(everything(), ~ suppressWarnings(sum(as.numeric(.), na.rm = TRUE))))

df_resultados$Comida_Primario <- df_resultados$Comida_Primario + df_resultados$Cafeteria_Secundario + df_resultados$Cafeteria_Terciario1 + df_resultados$Cafeteria_Adicional
df_resultados$Deporte_Primario_1 <- df_resultados$Deporte_Secundario
df_resultados$Deporte_Secundario <- df_resultados$Deporte_Primario_1
df_resultados$Deporte_Primario <- df_resultados$Deporte_Primario_1
df_resultados <- df_resultados[,-ncol(df_resultados)]


grid <- grid %>% left_join(df_resultados, by = "ID_CELL")


# Rellenar NAs en amenidades a 0
amenity_cols <- setdiff(names(df_resultados), "ID_CELL")
if (length(amenity_cols)) {
  grid <- grid %>% mutate(across(all_of(amenity_cols), \(x) replace_na(x, 0)))
}

# --- 2) Vecindarios
grid_sp <- as(grid, "Spatial")
Neighbours <- poly2nb(grid_sp, queen = T)

# --- 3) Pesos y función del índice (adaptada a tus nombres)
W1   <- 9  ## primaria
W2   <- 0.3 ## secundaria
W3   <- 0.2 ## terciaria
Wadd <- 0.1 ## adicionales
Wadd_max <- 0.3 ## ??
Wnb  <- 0.5 ## vecinos. Coeficiente reductor sobre los pesos anteriores
POP_VAR <- "TOT_P_2021"
##NO ES PERCAPITA
index_qol <- function(dataset_sf,
                      pop_var = "TOT_P_2021",
                      primary,
                      secondary,
                      tertiary,     
                      additional,    
                      PerCapita = F,
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
    geom_sf(aes_string(fill = var), color = "grey30") +
    scale_fill_viridis_c(
      option = "plasma",
      guide = guide_legend(title = "Index value"),
      limits = c(0, 10),              # fuerza el rango de la escala
      breaks = 1:10,                  # cortes discretos del 1 al 10
      labels = as.character(1:10)     # etiquetas en la leyenda
    ) +
    labs(title = title) +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5))
}

mapas <- list() 

p_general   <- plot_map(grid_qoli, "general",   "QOLI – Índice General")
p_dining    <- plot_map(grid_qoli, "dining",    "Índice de Comida")
p_transport <- plot_map(grid_qoli, "transport", "Índice de Transporte")
p_health    <- plot_map(grid_qoli, "health",    "Índice de Sanidad")
p_shopping  <- plot_map(grid_qoli, "shopping",  "Índice de Compras")
p_sport     <- plot_map(grid_qoli, "sport",     "Índice de Deporte")
p_edu       <- plot_map(grid_qoli, "education", "Índice de Educación")
p_nature    <- plot_map(grid_qoli, "nature",    "Índice de Naturaleza")
p_other     <- plot_map(grid_qoli, "other",     "Índice de Otros")

mapas <- list(
  p_general, p_dining, p_transport, p_health,
  p_shopping, p_sport, p_edu, p_nature, p_other
)

names(mapas) <- c(
  "p_general","p_comida","p_transporte","p_sanidad",
  "p_compras","p_deporte","p_educacion","p_naturaleza","p_otros"
)

library(gridExtra)

png("C:/Users/34633/Desktop/TFM/graficas/mapas_combinados.png",
    width = 4000, height = 4000, res = 300)

grid.arrange(
  grobs = mapas,
  ncol = 3,  # 3 columnas x 3 filas = 9 mapas
  top = "Índices QOLI – Municipio de Madrid"
)

dev.off()



setwd("C:/Users/34633/Desktop/TFM/graficas")

for(i in 1:length(mapas)){
  
  png(paste0("mapa_",names(mapas)[i],".png"), width = 4000, height = 3000,res = 600)  
  plot(mapas[[i]])
  dev.off()
  
}

# Muestra el general (puedes imprimir el resto igual)
print(p_general)

p_dining   ### esto
p_transport
p_health
p_shopping
p_sport
p_edu
p_nature   ### esto




