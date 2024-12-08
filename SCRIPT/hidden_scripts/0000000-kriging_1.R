################################################################################

#                                  Kriging_1

# https://pages.cms.hu-berlin.de/EOL/gcg_quantitative-methods/Lab14_Kriging.html
################################################################################
# Carica i pacchetti necessari
library(terra)
library(sp)
library(gstat)
library(sf)
library("mapview") # map plot
library("stars")   # spatial-temporal data

# Carica il file di occorrenza delle specie
DataSpecies_0 <- read_sf("./INPUT/VECTOR/p-psa_adj.gpkg")

# Rimuovi i valori mancanti dal dataframe DataSpecies_0
DataSpecies_0 <- na.omit(DataSpecies_0)
DataSpecies <- subset(DataSpecies_0, presence == 1)

# Visualizza la mappa utilizzando mapview
mapview(DataSpecies)

# Carica il raster ambientale a 50 m di risoluzione spaziale
myExpl_00 <- rast("./INPUT/RASTER/environmental_50m.tiff")

selected_bands <- c("green", "BIO03", "BIO06", "wo", "rivers", "fla", "soc", "roads")
myExpl_0 <- subset(myExpl_00, selected_bands)

myExpl <- st_as_stars(myExpl_0)

myExpl_split <- split(myExpl, "band")
myExpl

bbox <- st_bbox(myExpl_split)
bbox

cell_size <- 50

x <- seq(bbox$xmin, bbox$xmax, by=cell_size)
y <- seq(bbox$ymin, bbox$ymax, by=cell_size)

grid <- expand.grid(x=x, y=y)
plot(grid$x, grid$y, pch=19, cex=0.1)

grid$tmp <- 1
grid <- st_as_stars(grid, crs=st_crs(myExpl))
st_crs(grid) <- st_crs(myExpl) # re-assign crs to be safe


zn.idw <- idw(presence ~ 1, locations = DataSpecies, newdata = myExpl_split, idp = 1)
zn.idw

plot(rast(zn.idw["var1.pred"]))
plot(DataSpecies["presence"], col="red", cex=1, add=T, type="p")

mapview(zn.idw, zcol='var1.pred', layer.name = "Zinc ppm")

myExpl_1 <- rast(myExpl)

# Estrai i valori ambientali
environment_values <- terra::extract(myExpl_1, DataSpecies, method="simple", 
                                     xy = TRUE)

# Converti il data frame in un oggetto SpatialPointsDataFrame
coordinates(environment_values) <- c("x", "y")

# Assegna una CRS agli oggetti SpatialPointsDataFrame
proj4string(environment_values) <- CRS("+proj=utm +zone=32 +datum=WGS84 +units=m")

environment_values$presence <- 1
gstat::hscat(log(green) ~ 1, environment_values, (0:9) * 100)

# Stimare il modello variografico
variogram_model <- variogram(presence ~ 1, locations = DataSpecies)

# Plotta il variogramma
plot(variogram_model)

# Fitta il modello variografico
fit_variogram <- fit.variogram(variogram_model, model = vgm(1, "Sph", 100, 0.1))

# Effettua il kriging
kriged_model <- krige(presence ~ 1, locations = DataSpecies, model = fit_variogram)

# Puoi visualizzare il risultato
plot(kriged_model)


# Esegui il kriging universale
multiv_kriging <- krige(
  formula = presence ~ green + BIO03 + BIO06 + wo + rivers + fla + soc + roads,
  locations = environment_values,
  newdata = grid
)



