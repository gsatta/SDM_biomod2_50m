################################################################################

#                       Prepare Environmental raster data

################################################################################

# Load the necessary packages
library(terra)

# Load the environmental raster  at 50 m of spatial resolution
myExpl_0 <- rast("./INPUT/RASTER/environmental_50m.tif")

# # # Get only the selected variables
# myExpl <- subset(myExpl_0, c("nitrogen", "silt",
#                              "rivers", "roads", 'aspect',
#                              'BIO16',  "sand", "clay")) 


# # # Get only the selected variables
# myExpl_000 <- subset(myExpl_0, c("silt", "rivers", "roads",
#                              "aspect","sand", "clay",
#                               "soc", "twi", "tpi",
#                                "fla", "green", "slope", 
#                              "BIO3", "BIO4", "BIO06"  )) 


# # Get only the selected variables
myExpl <- subset(myExpl_0, c("green", "BIO3",
                             "soc", "sand", 'silt',
                             'roads',  "BIO06", "twi"))
