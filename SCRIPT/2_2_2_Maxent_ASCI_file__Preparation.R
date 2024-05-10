library(terra); library(sf)

# Carica il raster
myExpl_0 <- rast("./INPUT/RASTER/environmental_50m.tif")

mask <- read_sf("./INPUT/VECTOR/maskWO.gpkg")

# Crea la directory se non esiste già
output_dir <- "./INPUT/RASTER/ASCII/"  ### Modificare il nome se sto lavornando con i standardizzari o meno
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

# Estrai e salva ogni singolo layer in formato ASCII
for (i in 1:nlyr(myExpl_0)) {
  layer <- myExpl_0[[i]]
  layer_name <- names(myExpl_0)[i]
  clipped <- crop(layer, mask)
  output_file <- paste0(output_dir, layer_name, ".asc")
  terra::writeRaster(layer, filename = output_file, overwrite=TRUE, NAflag = -9999)
}


