################################################################################

#                       Prepare Environmental raster data

################################################################################

# Load the necessary packages
library(terra)

# Load the environmental raster  at 50 m of spatial resolution
myExpl_0 <- rast("./INPUT/RASTER/enviromental_50m.tiff")

# # Get only the selected variables
myExpl <- subset(myExpl_0, c("nitrogen", "sand", "fla",
                             "rivers", "roads", 'aspect',
                             'BIO16')) 
