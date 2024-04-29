library(terra)
library(sf)
library(dplyr)

# I file sono stati tutti ottenuti utilizzando SAGA sul computer

dem <- rast("./INPUT/RASTER/TOPOGRAPHIC/dtm10m_clipped_paul.tif")

writeRaster(dem, filename = "./INPUT/RASTER/TOPOGRAPHIC/dem.tif")

slope <- terrain(dem, v="slope", neighbors=8, unit="degrees")

# Salva lo stack raster come GeoTIFF
writeRaster(slope, filename = "./INPUT/RASTER/TOPOGRAPHIC/slope.tif")

aspect <- terrain(dem, v="aspect", neighbors=8, unit="degrees")

writeRaster(aspect, filename = "./INPUT/RASTER/TOPOGRAPHIC/aspect.tif")

###################

# Calcolo del twi su saga-gis usando il tutorial a questo link:
# https://www.youtube.com/watch?v=48mPD8mzXiQ

twi <- rast("./INPUT/RASTER/TOPOGRAPHIC/twi.tif")

############Crea un raster stack con tutti i file topogrphic
library(terra)

raster_folder <- "./INPUT/RASTER/TOPOGRAPHIC"

# Elenco dei file presenti nella cartella
raster_files <- list.files(raster_folder, pattern = "\\.tif$", full.names = TRUE)

# raster_files <- raster_files[-4]

raster_names <- basename(raster_files)

# Crea un raster di riferimento utilizzando il primo raster
reference_raster <- rast(raster_files[1])

# Leggi e allinea gli altri raster al raster di riferimento
aligned_rasters <- lapply(raster_files, function(file) {
  raster <- rast(file)
  aligned_raster <- resample(raster, reference_raster)
  return(aligned_raster)
})

# Crea lo stack raster con i raster allineati
topographic <- rast(aligned_rasters)

# Modifica i nomi dei layer
names(topographic) <- c("aspect", "dem", "slope", "twi")

# Visualizzare lo stack raster
topographic

mask <- read_sf("./INPUT/VECTOR/mask.gpkg")

# clip the data usising the wild olive mask
topographic_clip <- crop(topographic, mask, snap="near", mask=TRUE, touches=TRUE, extend=FALSE)

# Specifica il percorso e il nome del file di output GeoTIFF
output_file_geotiff <- "./INPUT/RASTER/TOPOGRAPHIC/STACK/topographic_stack.tif"

# Salva lo stack raster come GeoTIFF
writeRaster(topographic, filename = output_file_geotiff, overwrite=TRUE)


a <- rast(raster_files[[1]])
b <- rast(raster_files[[2]])
c <- rast(raster_files[[3]])
d <- rast(raster_files[[4]])





