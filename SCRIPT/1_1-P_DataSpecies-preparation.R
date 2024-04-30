################################################################################

#                         DataSpecies preparation

###############################################################################

# Load the necessary packages
library(sf); library(readr); library(spatialsample); library(ggplot2)

# Load specie occurrences file
DataSpecies_0 <- st_read("./INPUT/VECTOR/p-psa_adj.gpkg")

DataSpecies <- subset(DataSpecies_0, presence == 1)

# Carica il grid A 50 m
grid <- read_sf("./INPUT/VECTOR/reticolo_50m_mascked.gpkg")

# Intersezione tra i punti di DataSpecies e le celle del raster
intersection <- st_intersection(DataSpecies, grid)

# Rimuovi i duplicati basati sulle coordinate della cella del raster
unique_points <- intersection[!duplicated(intersection$id), ]

# Estrai le coordinate x e y
x <- st_coordinates(unique_points$geom)[, 1]
y <- st_coordinates(unique_points$geom)[, 2]

# Aggiungi le coordinate x e y al dataframe
unique_points$x <- x
unique_points$y <- y

write_sf(unique_points, "./INPUT/VECTOR/p-psa_adj_unique_points.gpkg")

# Convert the layer in dataframe
DataSpecies_df <- as.data.frame(unique_points)

# Delete the geom column
DataSpecies_df$geom <- NULL

write_csv(DataSpecies_df, "./INPUT/CSV/p-psa_adj.csv")
# #--------------------------

lim <- read_sf("./INPUT/VECTOR/limite_amministrativo_paulilatino_32632.gpkg")

set.seed(1234)

#  Separa il  train dal test
# Create splits
splits <- spatial_clustering_cv(
  data = unique_points,
  v = 5
)

# Visualize them
autoplot(splits)

# Get the Data
train_data <- analysis(splits$splits[[1]])
test_data <- assessment(splits$splits[[1]])

# Definisci i colori e le etichette per la legenda
colors <- c("green", "red")
labels <- c("Train Data", "Test Data")

# Crea il grafico e aggiungi i dati
ggplot() +
  geom_sf(data = lim) +
  geom_sf(data = train_data, aes(color = "Train Data")) +
  geom_sf(data = test_data, aes(color = "Test Data")) +
  scale_color_manual(name = "Data",
                     values = colors,
                     labels = labels)

# Save the train
write_sf(train_data, "./INPUT/VECTOR/train_data.gpkg")
# Save the test
write_sf(test_data, "./INPUT/VECTOR/test_data.gpkg")

# ######################  train preparation   ############################
# 
# # Estrai le coordinate x e y
# x_train <- st_coordinates(train_data$geom)[, 1]
# y_train <- st_coordinates(train_data$geom)[, 2]
# 
# # Aggiungi le coordinate x e y al dataframe
# train_data$x <- x_train
# train_data$y <- y_train
# 
# # Convert the layer in dataframe
# train_df <- as.data.frame(train_data)
# 
# # Delete the geom column
# train_df$geom <- NULL
# 
# # Save the csv file
# write_csv(train_df, "./INPUT/CSV/train_df.csv")
# 
# ######################  test preparation   ############################
# 
# # Estrai le coordinate x e y
# x_test <- st_coordinates(test_data$geom)[, 1]
# y_test <- st_coordinates(test_data$geom)[, 2]
# 
# # Aggiungi le coordinate x e y al dataframe
# test_data$x <- x_test
# test_data$y <- y_test
# 
# # Convert the layer in dataframe
# test_df <- as.data.frame(test_data)
# 
# # Delete the geom column
# test_df$geom <- NULL
# 
# # Save the csv file
# write_csv(test_df, "./INPUT/CSV/test_df.csv")
