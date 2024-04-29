library(biomod2)
library(terra)
library(readr)


###  whit the cross validation #####

# Load specie occurrences
DataSpecies <- read.csv("./INPUT/CSV/p-psa.csv")

# Select only the presence data
DataSpecies <- subset(DataSpecies, presence == 1)

# Converti la colonna 'presence' in numerico
DataSpecies$presence <- as.numeric(DataSpecies$presence)

# Split della colonna geom per ottenere le coordinate
coordinates <- strsplit(as.character(DataSpecies$geom), ", ")

# Estrai le coordinate x e y
x <- sapply(coordinates, function(coord) as.numeric(gsub("c\\(", "", coord[1])))
y <- sapply(coordinates, function(coord) as.numeric(gsub("\\)", "", coord[2])))

# Crea un dataframe con le coordinate x e y
myRespXY <- data.frame(x, y)

############################################

# # Without Cross-Validation
# # Load specie occurrences
# train <- read_delim("./INPUT/CSV/train_df.csv", delim= ",")
# 
# # Select only the presence data
# train <- subset(train, presence == 1)
# 
# # Converti la colonna 'presence' in numerico usando la sintassi alternativa
# myResp_train <- as.numeric(train[["presence"]])
# 
# # Get corresponding XY coordinates
# myRespXY_train <- train[, c('x', 'y')]
# 
# ###########
# 
# # Load specie occurrences
# test <- read_delim("./INPUT/CSV/test_df.csv", delim= ",")
# 
# # Select only the presence data
# test <- subset(test, presence == 1)
# 
# # Converti la colonna 'presence' in numerico usando la sintassi alternativa
# myResp_test <- as.numeric(test[["presence"]])
# 
# # Get corresponding XY coordinates
# myRespXY_test <- test[, c('x', 'y')]

