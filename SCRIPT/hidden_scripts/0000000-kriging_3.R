


# Carica il file di occorrenza delle specie
DataSpecies_0 <- read_sf("./INPUT/VECTOR/p-psa_adj.gpkg")

# Rimuovi i valori mancanti dal dataframe DataSpecies_0
DataSpecies_0 <- na.omit(DataSpecies_0)
DataSpecies <- subset(DataSpecies_0, presence == 1)

DataSpecies <- st_coordinates(DataSpecies)
DataSpecies <- cbind(DataSpecies, DataSpecies$X, DataSpecies$Y)
colnames(DataSpecies) <- c("presence", "geom", "x", "y")

# Visualizza la mappa utilizzando mapview
mapview(DataSpecies)





library(gmGeostats)
plot(DataSpecies)

compositions::plot.acomp(DataSpecies)

make.gmMultivariateGaussianSpatialModel(
                  data = DataSpecies,
                  formula =  
                  coords = attr(DataSpecies$geom, "coords"))
