################################################################################

#                            Single Models Projection

################################################################################
library(sf); library(terra)

 #myBiomodEM Project ensemble models (building single projections)
myBiomodEMProj <- BIOMOD_EnsembleForecasting(bm.em = myBiomodEM,
                                             proj.name = 'CurrentEM_09-07-2024',
                                             new.env = myExpl,
                                             models.chosen = 'all',
                                             metric.binary = 'all',
                                             metric.filter = 'all',
                                             build.clamping.mask = TRUE,
                                             do.stack = TRUE,
                                             output.format = ".tif")
myBiomodEMProj
plot(myBiomodEMProj)

mappa <- rast("./Phytophthora/proj_CurrentEM_09-07-2024/proj_CurrentEM_09-07-2024_Phytophthora_ensemble.tif")
focolai <- read_sf("./INPUT/VECTOR/FOCOLAI.gpkg")
lim <- read_sf("./INPUT/VECTOR/limite_amministrativo_paulilatino_32632.gpkg")
unique_points <- read_sf("./INPUT/VECTOR/random_unique_points.gpkg")
# presences <- read_sf("./INPUT/VECTOR/random_points_presences.gpkg")
# absences <- read_sf("./INPUT/VECTOR/random_points_absences.gpkg")
mask_wo <- read_sf("./INPUT/VECTOR/maskWO_def_ok.gpkg")
rivers <- read_sf("./INPUT/VECTOR/idrografia.gpkg")
roads <- read_sf("./INPUT/VECTOR/Secondaria.gpkg")

mask_wo$wild_olive <- NULL
mask_wo$area <- NULL

# mappa_mean <- mappa$Phytophthora_EMmeanByROC_mergedData_mergedRun_mergedAlgo/1000

mappa_median <- mappa$Phytophthora_EMmedianByROC_mergedData_mergedRun_mergedAlgo/1000
# mappa_EMcv <- mappa$Phytophthora_EMcvByROC_mergedData_mergedRun_mergedAlgo

# Colori per i punti basati su 'presence'
# colors <- ifelse(DataSpecies_0$presence == 1, "red", "green")

# Plotta il raster
plot(mappa_median, col = terrain.colors(100))
# Sovrapposizione dei punti sul raster con i colori specificati
points(unique_points, col = c("black", "black")[unique_points$presence + 1], pch = 21, bg = c("green", "red")[unique_points$presence + 1], cex = 1)
# points(absences, col = "#99cbff", cex = 1)
# Disegna i bordi del poligono
# lines(focolai, col = "red")
lines(lim, col = "black")
lines(rivers, col = "blue", alpha= 0.5)
lines(roads, col = "black", alpha= 0.5)
polys(mask_wo, col = "gray",  alpha= 0.5)


library(mapview)
library(RColorBrewer)

# Definire la mappa
map <- mapview(mappa_median, 
               col.regions = rev(brewer.pal(11, "RdYlGn")), 
               na.color = "transparent",
               layer.name = "Median Ensemble map",
               Trim = TRUE) +
  mapview(unique_points, 
          col = unique_points$color, 
          legend = TRUE,
          burst = TRUE) +
  mapview(lim, 
          alpha.regions = 0, 
          col.regions = "transparent",
          lwd = 1,
          layer.name = "Limiti",
          legend = FALSE,
          popup = FALSE) +
  mapview(roads_sf, 
          alpha.regions = 0, 
          lwd = 1,
          layer.name = "roads",
          legend = FALSE,
          popup = FALSE) +
  mapview(rivers_sf, 
          alpha.regions = 0, 
          lwd = 1,
          layer.name = "Rivers",
          legend = FALSE,
          popup = FALSE)

# Visualizza la mappa
print(map)



library(ggplot2)
library(sf)
library(ggspatial)
library(terra) # utilizza il pacchetto 'terra' per la manipolazione dei raster
library(grid)

# Trasforma le geometrie in EPSG:32632
unique_points <- st_transform(unique_points, crs = 32632)
lim <- st_transform(lim, crs = 32632)

map <- ggplot() +
  # Plotta il raster con una palette meno vivace
  layer_spatial(data = mappa_median) +
  # Imposta la scala dei colori del raster con toni più tenui, con valori NA trasparenti
  scale_fill_gradientn(name = "Probability", colors = c("#d9f0d3", "#f7fcb9", "#e34a33"), na.value = "transparent") +
  # Aggiungi il limite amministrativo
  geom_sf(data = lim, aes(color = "#636363"), fill = NA, alpha = 0.7, show.legend = TRUE) +
  geom_sf(data = focolai, aes(color = "Outbreaks"), alpha = 0, show.legend = TRUE) +
  # Aggiungi i fiumi
  # geom_sf(data = rivers, aes(color = "rivers"), fill = NA, alpha = 0.7) +
  # # Aggiungi le strade secondarie
  # geom_sf(data = roads, aes(color = "roads"), fill = NA, alpha = 0.7) +
  # Plotta i punti con colori specifici e maggiore dimensione
  geom_sf(data = unique_points, aes(color = factor(presence)), size = 1.5, lwd = 1) +
  # Definisci i colori manualmente con toni meno vivaci e modifica il nome della legenda
  scale_color_manual(name = "Sampled points", 
                     values = c("0" = "green", "1" = "red", "Outbreaks" = "red"),
                     labels = c("0" = "Absence", "1" = "Presence", "lim" = "", "Outbreaks" = "Outbreaks")) +
  # Aggiungi la barra di scala in metri
  annotation_scale(location = "bl", text_cex = 1, style = "ticks") +
  # Migliora la leggibilità del tema
  theme_test(base_size = 15) +
  theme(
    legend.position = "right",
    axis.text = element_text(size = 10),
    axis.title.x = element_blank(), # Rimuove il titolo dell'asse x
    axis.title.y = element_blank(), # Rimuove il titolo dell'asse y
    legend.text = element_text(size = 12), # Riduce le dimensioni del testo della legenda
    legend.title = element_text(size = 14) # Riduce le dimensioni del titolo della legenda
  ) +
  guides(
    fill = guide_legend(order = 1), # Assegna l
    color = guide_legend(order = 3) # Assegna l'ordine 2 alla legenda "Sampling points" e "Outbreaks"
  ) +
  coord_sf(crs = st_crs(32632), datum = st_crs(32632), expand = FALSE) + # Imposta il sistema di coordinate in EPSG:32632
  # Aggiungi testo personalizzato in basso a destra
  annotate("text", x = Inf, y = -Inf, label = "WGS 84 / UTM zone 32N",
           hjust = 1.1, vjust = -0.75, size = 3.5) +
  # Aggiungi la bussola nord
  annotation_north_arrow(location = "tr", which_north = "true", style = north_arrow_fancy_orienteering())

# Mostra il grafico
print(map)

# # Aggiunta del poligono mask_wo con riempimento e griglia trasparenti
# map <- map +
#   geom_sf_pattern(data = mask_wo, pattern = "crosshatch", pattern_color = "black", 
#                   pattern_density = 0.1, pattern_spacing = 0.02, pattern_alpha = 0.2, 
#                   fill = "transparent", alpha = 0, color = "gray") +
#   geom_sf(data = lim, color = "#636363", fill = NA, alpha = 0.7) 
  
## Visualizzazione della mappa
# print(map)

# Salva il grafico in un file nella cartella ./GRAPHS/
ggsave("./GRAPHS/map_probability_09-07-24_3.jpg", plot = map, width = 10, height = 7, dpi = 500)





