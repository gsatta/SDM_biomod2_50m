################################################################################

#                       Prepare Environmental raster data

################################################################################

# Load the necessary packages
library(terra)

# Load the environmental raster  at 50 m of spatial resolution
myExpl_0 <- rast("./INPUT/RASTER/environmental_50m_standardized.tif")

# myExpl_0_scaled <- scale(myExpl_0, center=TRUE, scale=TRUE)

# # Get only the selected variables
myExpl <- subset(myExpl_0, c("nitrogen", "sand", "fla",
                             "rivers", "roads", 'aspect',
                             'BIO16')) 
