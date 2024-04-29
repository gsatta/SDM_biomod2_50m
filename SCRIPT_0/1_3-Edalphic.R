library(terra)
library(sf)
library(dplyr)

raster_folder <- "./INPUT/RASTER/EDALPHIC"

# Elenco dei file presenti nella cartella
raster_files <- list.files(raster_folder, pattern = "\\.tif$", full.names = TRUE)

raster_names <- basename(raster_files)

# Creazione dello stack raster
edalphic <- rast(raster_files)

# Visualizzare lo stack raster
edalphic

# Specifica il percorso e il nome del file di output GeoTIFF
output_file_geotiff <- "./INPUT/RASTER/EDALPHIC/STACK/edalphic_stack.tif"

# Salva lo stack raster come GeoTIFF
writeRaster(edalphic, filename = output_file_geotiff)

