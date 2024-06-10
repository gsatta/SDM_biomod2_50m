################################################################################

#                            Single Models Projection

################################################################################
library(sf); library(terra)

# # Projsftime# # Project ensemble models (building single projections)
myBiomodEMProj <- BIOMOD_EnsembleForecasting(bm.em = myBiomodEM,
                                             proj.name = 'CurrentEM_05-06-2024',
                                             new.env = myExpl,
                                             models.chosen = 'all',
                                             metric.binary = 'all',
                                             metric.filter = 'all',
                                             build.clamping.mask = TRUE,
                                             do.stack = TRUE,
                                             output.format = ".tif")
myBiomodEMProj
plot(myBiomodEMProj)

mappa <- rast("./Phytophthora/proj_CurrentEM_05-06-2024/proj_CurrentEM_05-06-2024_Phytophthora_ensemble.tif")
focolai <- read_sf("./INPUT/VECTOR/FOCOLAI.gpkg")
lim <- read_sf("./INPUT/VECTOR/limite_amministrativo_paulilatino_32632.gpkg")
presences <- read_sf("./INPUT/VECTOR/random_points_presences.gpkg")
absences <- read_sf("./INPUT/VECTOR/random_points_absences.gpkg")
mask_wo <- read_sf("./INPUT/VECTOR/maskWO_def_ok.gpkg")

mask_wo$wild_olive <- NULL
mask_wo$area <- NULL

# mappa_mean <- mappa$Phytophthora_EMmeanByROC_mergedData_mergedRun_mergedAlgo/1000

mappa_median <- mappa$Phytophthora_EMmedianByROC_mergedData_mergedRun_mergedAlgo/1000
mappa_EMcv <- mappa$Phytophthora_EMcvByROC_mergedData_mergedRun_mergedAlgo

# Colori per i punti basati su 'presence'
# colors <- ifelse(DataSpecies_0$presence == 1, "red", "green")

# Plotta il raster
plot(mappa_median, col = terrain.colors(100))
# Sovrapposizione dei punti sul raster con i colori specificati
points(presences, col = "red", cex = 1)
points(absences, col = "#99cbff", cex = 1)
# Disegna i bordi del poligono
# lines(focolai, col = "red")
lines(lim, col = "black")
polys(mask_wo, col = "gray",  alpha= 0.5)


library(mapview)
library(RColorBrewer)

# Visualizza la mappa utilizzando mapview
mapview(mappa_median, 
        col.regions = rev(brewer.pal(11, "RdYlGn")), 
        na.color = "transparent",
        layer.name = "Median Ensemble map",
        Trim = TRUE)+
  mapview(presences, 
          col.regions = "red", 
          layer.name = "Occurences",
          popup = F) +
  mapview(absences, 
          col.regions = "green", 
          layer.name = "Absences",
          popup = F) +
  mapview(lim, 
          alpha.regions = 0, 
          col.regions = "transparent",
          lwd = 1,
          layer.name = "Limiti",
          legend = FALSE,
          popup = F)  


library(ggplot2)
library(ggspatial)
library(sf)
library(ggpattern)

# Creazione del grafico base
map <- ggplot() +
  # Plotta il raster con una palette meno vivace
  layer_spatial(mappa_median) +
  # Imposta la scala dei colori del raster con toni più tenui, con valori NA trasparenti
  scale_fill_gradientn(name = "Probability", colors = c("#d9f0d3", "#f7fcb9", "#e34a33"), na.value = "transparent") +
  # Plotta i punti con colori specifici e maggiore dimensione
  geom_sf(data = DataSpecies_0, aes(color = factor(presence)), size = 3) +
  # Definisci i colori manualmente con toni meno vivaci e modifica il nome della legenda
  scale_color_manual(name = "Occurrences", values = c("0" = "green", "1" = "red")) +
  # Aggiungi il limite amministrativo
  geom_sf(data = lim, color = "#636363", fill = NA, alpha = 0.7) +
  # Aggiungi la barra di scala in metri
  annotation_scale(location = "bl", text_cex = 1, style = "ticks") +
  # Migliora la leggibilità del tema
  theme_test(base_size = 15) +
  theme(
    legend.position = "right",
    axis.text = element_text(size = 10),
    axis.title = element_text(size = 14),
    legend.text = element_text(size = 12), # Riduce le dimensioni del testo della legenda
    legend.title = element_text(size = 14) # Riduce le dimensioni del titolo della legenda
  )

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
ggsave("./GRAPHS/map_probability.jpg", plot = map, width = 10, height = 7, dpi = 500)





