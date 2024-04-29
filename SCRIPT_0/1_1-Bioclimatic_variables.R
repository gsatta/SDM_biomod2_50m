library(terra)
library(sf)
library(dplyr)


raster_folder <- "./INPUT/RASTER/BIOCLIM"

# Elenco dei file presenti nella cartella
raster_files <- list.files(raster_folder, pattern = "\\.tif$", full.names = TRUE)

raster_names <- basename(raster_files)

# Creazione dello stack raster
bioclim_current <- rast(raster_files)

# Assegnare i nuovi nomi alle bande dello stack raster
names(bioclim_current) <- raster_names

# Visualizzare lo stack raster
bioclim_current

# Specifica il percorso e il nome del file di output GeoTIFF
output_file_geotiff <- "./INPUT/RASTER/BIOCLIM/STACK/bioclim_current.tif"

# Salva lo stack raster come GeoTIFF
writeRaster(bioclim_current, filename = output_file_geotiff)

