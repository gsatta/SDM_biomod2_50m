################################################################################

#                                  Kriging_0

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

# Carica il raster ambientale a 50 m di risoluzione spaziale
myExpl_00 <- rast("./INPUT/RASTER/environmental_50m.tiff")
myExpl_0 <- subset(myExpl_00, c("green", "BIO03", "BIO06", "wo", "rivers", "fla", "soc", "roads"))

# Assegna una CRS diretta al raster
myExpl_0 <- terra::crs(myExpl_0) <- "+proj=utm +zone=32 +datum=WGS84 +units=m"

myExpl <- read_stars(myExpl_0)

# Estrai i valori ambientali
environment_values <- terra::extract(myExpl, DataSpecies, method="simple", 
                                     xy = TRUE)

# Converti il data frame in un oggetto SpatialPointsDataFrame
coordinates(environment_values) <- c("x", "y")

# Assegna una CRS agli oggetti SpatialPointsDataFrame
proj4string(environment_values) <- CRS("+proj=utm +zone=32 +datum=WGS84 +units=m")

# Visualizza la mappa utilizzando mapview
mapview(environment_values)

# Filtra le variabili con dati
environment_values_filtered <- Filter(function(x) !is.null(x), environment_values)

# Converti DataSpecies in un oggetto SpatialPointsDataFrame
coords <- st_coordinates(DataSpecies)
data <- as.data.frame(DataSpecies)

bbox <- st_bbox(myExpl)
bbox

cell_size <- 50

x <- seq(bbox$xmin, bbox$xmax, by=cell_size)
y <- seq(bbox$ymin, bbox$ymax, by=cell_size)

meuse_grid <- expand.grid(x=x, y=y)
plot(meuse_grid$x, meuse_grid$y, pch=19, cex=0.1)

meuse_grid$tmp <- 1
meuse_grid <- st_as_stars(meuse_grid, crs=st_crs(myExpl))

# # Assegna una CRS agli oggetti SpatRaster
# crs(myExpl_0) <- CRS("+proj=utm +zone=32 +datum=WGS84 +units=m")

# Esegui l'interpolazione IDW
zn.idw <- gstat::idw(presence ~ 1, locations=DataSpecies, newdata=myExpl, idp = 2)

# Visualizza il risultato
zn.idw












# Unisci i valori estratti ai dati
for (var in names(environment_values_filtered)) {
  data[[var]] <- environment_values_filtered[[var]]
}

# Crea SpatialPointsDataFrame
sp_data <- SpatialPointsDataFrame(coords, data, proj4string=CRS(st_crs(DataSpecies)$proj4string))

# Assicurati che l'ID sia presente
sp_data$ID <- 1:nrow(sp_data)

# Funzione per eseguire il kriging
kriging_function <- function(var) {
  # Calcola il variogramma con scaling delle distanze
  variogram_data <- variogram(as.formula(paste(var, "~ 1")), locations = sp_data, cutoff = 10000) # Scala la distanza
  variogram_model <- fit.variogram(variogram_data, vgm(model="Exp"))
  
  # Verifica il variogramma
  print(variogram_model)
  
  # Verifica se il variogramma è valido (psill non zero)
  if (variogram_model$psill[2] > 0) {
    # Esegui il kriging
    kriging_result <- krige(formula = as.formula(paste(var, "~ 1")), 
                            locations = sp_data, 
                            newdata = sp_data, 
                            model = variogram_model)
    return(kriging_result)
  } else {
    warning(paste("Variogram model for", var, "is not valid. Psill is zero."))
    return(NULL)
  }
}

# Esegui il kriging per ciascuna variabile
modelli_kriging <- lapply(names(environment_values_filtered), kriging_function)

# Rimuovi eventuali risultati NULL
modelli_kriging <- Filter(Negate(is.null), modelli_kriging)

# Verifica il risultato
modelli_kriging





