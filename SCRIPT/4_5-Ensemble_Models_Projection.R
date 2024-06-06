################################################################################

#                            Single Models Projection

################################################################################
library(sf)

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

# mappa_mean <- mappa$Phytophthora_EMmeanByROC_mergedData_mergedRun_mergedAlgo/1000

mappa_median <- mappa$Phytophthora_EMmedianByROC_mergedData_mergedRun_mergedAlgo/1000
mappa_EMcv <- mappa$Phytophthora_EMcvByROC_mergedData_mergedRun_mergedAlgo

# Colori per i punti basati su 'presence'
# colors <- ifelse(DataSpecies_0$presence == 1, "red", "green")

# Plotta il raster
plot(mappa_median, col = terrain.colors(100), main = "Median Ensemble map" )
# Sovrapposizione dei punti sul raster con i colori specificati
points(presences, col = "red", cex = 1)
points(absences, col = "#99cbff", cex = 1)
# Disegna i bordi del poligono
# lines(focolai, col = "red")
lines(lim, col = "black")


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


# library(ggspatial)istrict", burst = TRUE)

# # Crea il plot
# ggplot() +
#   # Plotta il raster
#   layer_spatial(mappa_median) +
#   # Imposta la scala dei colori del raster da verde per 0 a rosso per 1
#   scale_fill_gradient(low = "red", high = "green") +
#   # Plotta i punti con colori specifici
#   geom_sf(data = DataSpecies_0, aes(color = factor(presence))) +
#   scale_color_manual(values = c("0" = "green", "1" = "red")) +
#   # Plotta i bordi del poligono
#   geom_sf(data = focolai, color = "red", fill = NA) +
#   geom_sf(data = lim, color = "black", fill = NA) +
#   # Titolo del grafico
#   ggtitle("Ensemble map") +
#   theme_minimal() +
#   theme(legend.position = "right")


