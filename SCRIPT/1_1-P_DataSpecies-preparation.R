################################################################################

#                         DataSpecies preparation

###############################################################################

# Load the necessary packages
library(sf); library(readr); library(spatialsample); library(ggplot2); 
library(spThin); library(dplyr)

set.seed(1234)
# Load specie occurrences file
DataSpecies_0 <- st_read("./INPUT/VECTOR/p-psa_adj.gpkg")

# Rimuovi i valori mancanti dal dataframe DataSpecies_0
DataSpecies_0 <- na.omit(DataSpecies_0)

absences <- subset(DataSpecies_0, presence == 0)
DataSpecies <- subset(DataSpecies_0, presence == 1)

# Carica il grid A 200 m
# grid <- read_sf("./INPUT/VECTOR/reticolo_200m_filtered.gpkg")

grid <- read_sf("./INPUT/VECTOR/reticolo_250m.gpkg")

# Funzione per selezionare casualmente un punto da ciascuna cella della griglia
select_random_points <- function(grid, species_data) {
  # Suddividi i dati della specie in base alla cella della griglia
  species_in_grid <- st_join(species_data, grid)
  
  # Seleziona casualmente un punto da ciascuna cella della griglia
  random_points <- species_in_grid %>%
    group_by(id) %>%
    sample_n(1) %>%
    ungroup()
  
  return(random_points)
}

# Utilizza la funzione per selezionare casualmente un punto da ciascuna cella della griglia
random_points_presences <- select_random_points(grid, DataSpecies)

random_points_absences <- select_random_points(grid, absences)

lim <- read_sf("./INPUT/VECTOR/limite_amministrativo_paulilatino_32632.gpkg")

# Visualizza i punti casuali selezionati insieme alla griglia e ai dati delle specie
ggplot() +
  geom_sf(data = lim, fill = NA, color = "black") +
  geom_sf(data = grid, fill = NA, color = "black") +
  # geom_sf(data = DataSpecies, color = "blue", size = 3, shape = 20) +
  geom_sf(data = random_points_presences, color = "red", size = 3, shape = 20) +
  geom_sf(data = random_points_absences, color = "green", size = 3, shape = 20) +
  theme_minimal() +
  labs(title = "Selected points",
       x = "Longitude",
       y = "Latitude")

write_sf(random_points_presences, "./INPUT/VECTOR/random_points_presences.gpkg")

write_sf(random_points_absences, "./INPUT/VECTOR/random_points_absences.gpkg")

# Unisci i due insiemi di dati
unique_points <- rbind(random_points_presences, random_points_absences)

write_sf(unique_points, "./INPUT/VECTOR/random_unique_points.gpkg")

##################################
# # Intersezione tra i punti di DataSpecies e le celle del raster
# intersection_P <- st_intersection(DataSpecies, grid)
# # Rimuovi i duplicati basati sulle coordinate della cella del raster
# unique_points_P <- intersection_P[!duplicated(intersection_P$id), ]
# 
# # Intersezione tra i punti di DataSpecies e le celle del raster
# intersection_A <- st_intersection(absences, grid)
# # Rimuovi i duplicati basati sulle coordinate della cella del raster
# unique_points_A <- intersection_A[!duplicated(intersection_A$id), ]
# # Unisci i due insiemi di dati
# unique_points <- rbind(unique_points_P, unique_points_A)

#############

# Calcola il numero di punti da estrarre (70% dei dati)
num_presence_1 <- round(nrow(random_points_presences) * 0.70)

# Estrai casualmente 74 punti con presence == 1
train_data <- unique_points %>%
  filter(presence == 1) %>%
  sample_n(num_presence_1, replace = FALSE)

# Estrai casualmente il restante 30% dei dati, inclusi i punti con presence == 0 e i punti con presence == 1 rimanenti
test_data <- unique_points %>%
  filter(!(id %in% train_data$id)) %>%
  sample_n(nrow(unique_points) - nrow(train_data), replace = FALSE)

train <- train_data %>%
  select(c(geom, presence))

test <- test_data %>%
  select(c(geom, presence))

lim <- read_sf("./INPUT/VECTOR/limite_amministrativo_paulilatino_32632.gpkg")

# Crea il grafico e aggiungi i dati
ggplot() +
  geom_sf(data = lim) +
  geom_sf(data = train_data, aes(color = "Train Data")) +
  geom_sf(data = test_data, aes(color = "Test Data")) 

# Save the train
write_sf(train, "./INPUT/VECTOR/train_data.gpkg")
# Save the test
write_sf(test, "./INPUT/VECTOR/test_data.gpkg")

# # Estrai le coordinate x e y
# x <- st_coordinates(unique_points$geom)[, 1]
# y <- st_coordinates(unique_points$geom)[, 2]
# 
# # Aggiungi le coordinate x e y al dataframe
# unique_points$x <- x
# unique_points$y <- y
# 
# write_sf(unique_points, "./INPUT/VECTOR/p-psa_adj_unique_points.gpkg")
# 
# # Convert the layer in dataframe
# DataSpecies_df <- as.data.frame(unique_points)
# 
# # Delete the geom column
# DataSpecies_df$geom <- NULL
# 
# write_csv(DataSpecies_df, "./INPUT/CSV/p-psa_adj.csv")
# #--------------------------


# set.seed(9999)
# 
# #  Separa il  train dal test
# # Create splits
# splits <- spatial_clustering_cv(
#   data = unique_points_P,
#   v = 5,
#   repeats = 5
# )
# 
# # Visualize them
# autoplot(splits)
# 
# # Get the Data
# train_data <- analysis(splits$splits[[1]])
# test_data_0 <- assessment(splits$splits[[1]])
# 
# # Unisci i due insiemi di dati
# test_data <- rbind(test_data_0, unique_points_A)
# 
# # Definisci i colori e le etichette per la legenda
# colors <- c("green", "red")
# labels <- c("Test Data", "Train Data")
# 
# # Crea il grafico e aggiungi i dati
# ggplot() +
#   geom_sf(data = lim) +
#   geom_sf(data = train_data, aes(color = "Train Data")) +
#   geom_sf(data = test_data, aes(color = "Test Data")) 
# 
# train <- train_data %>%
#   select(c(geom, presence))
# 
# test <- test_data %>%
#   select(c(geom, presence))
# 
# # Save the train
# write_sf(train, "./INPUT/VECTOR/train_data.gpkg")
# # Save the test
# write_sf(test, "./INPUT/VECTOR/test_data.gpkg")
# 
# ######################  train preparation   ############################
# 
# # # Estrai le coordinate x e y
# # x_train <- st_coordinates(train_data$geom)[, 1]
# # y_train <- st_coordinates(train_data$geom)[, 2]
# # 
# # # Aggiungi le coordinate x e y al dataframe
# # train_data$x <- x_train
# # train_data$y <- y_train
# # 
# # # Convert the layer in dataframe
# # train_df <- as.data.frame(train_data)
# # 
# # # Delete the geom column
# # train_df$geom <- NULL
# # 
# # # Save the csv file
# # write_csv(train_df, "./INPUT/CSV/train_df.csv")
# # 
# # ######################  test preparation   ############################
# # 
# # # Estrai le coordinate x e y
# # x_test <- st_coordinates(test_data$geom)[, 1]
# # y_test <- st_coordinates(test_data$geom)[, 2]
# # 
# # # Aggiungi le coordinate x e y al dataframe
# # test_data$x <- x_test
# # test_data$y <- y_test
# # 
# # # Convert the layer in dataframe
# # test_df <- as.data.frame(test_data)
# # 
# # # Delete the geom column
# # test_df$geom <- NULL
# # 
# # # Save the csv file
# # write_csv(test_df, "./INPUT/CSV/test_df.csv")
# 
