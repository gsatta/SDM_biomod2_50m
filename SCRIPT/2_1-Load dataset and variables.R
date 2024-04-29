
library(biomod2)
library(terra)
library(readr)

###  whit the cross validation #####
# TRAIN
# Load train specie occurrences
train <- read_csv("./INPUT/CSV/train_df.csv")

# Select only the presence data
train <- subset(train, presence == 1)

# Converti la colonna 'presence' in numerico usando la sintassi alternativa
myResp_train <- as.numeric(train[["presence"]])

# Get corresponding XY coordinates
myRespXY_train <- train[, c('x', 'y')]

##### TEST ####
# Load test specie occurrences
test <- read_delim("./INPUT/CSV/test_df.csv", delim= ",")

# Select only the presence data
test <- subset(test, presence == 1)

# Converti la colonna 'presence' in numerico usando la sintassi alternativa
myResp_test <- as.numeric(test[["presence"]])

# Get corresponding XY coordinates
myRespXY_test <- test[, c('x', 'y')]

