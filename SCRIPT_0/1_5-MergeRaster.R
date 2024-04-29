# 1_5-MergeRaster

bioclim_current <- rast("./INPUT/RASTER/BIOCLIM/STACK/bioclim_current.tif")

topographic <- rast("./INPUT/RASTER/TOPOGRAPHIC/STACK/topographic_stack.tif")

edalphic <- rast("./INPUT/RASTER/EDALPHIC/STACK/edalphic_stack.tif")

# Unisci i due raster
multilayer <- c(edalphic, topographic)
