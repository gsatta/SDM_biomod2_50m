################################################################################

#                                 Load Data Set

################################################################################
# Load the necessary packages
library(readr); library(sf); library(terra)

# Clear the environment
rm(list = ls())

# 
# # Clear the environment
# rm(list = ls())
# 
# # Load specie occurrences
# DataSpecies <- read_delim("./INPUT/CSV/p-psa_adj.csv", delim= ",")
# 
# # Select only the presence data
# DataSpecies <- subset(DataSpecies, presence == 1)
# 
# # Select only the "presence" column and convert it in numeric
# myResp <- as.numeric(DataSpecies[["presence"]])
# 
# # Get corresponding XY coordinates
# myRespXY <- DataSpecies[, c('x', 'y')]

####################################################

###  whit the cross validation and the train and test data#####

# Clear the environment
rm(list = ls())

myResp_train <- vect("./INPUT/VECTOR/train_data.gpkg")
 
myResp_test <- vect("./INPUT/VECTOR/test_data.gpkg")




######################

# 
# # TRAIN
# # Load train specie occurrences
# train <- read_csv("./INPUT/CSV/train_df.csv")
# 
# # Select only the "presence" column and convert it in numeric
# myResp_train <- as.numeric(train[["presence"]])
# 
# # Get corresponding XY coordinates
# myRespXY_train <- train[, c('x', 'y')]
# 
# ##### TEST ####
# # Load test specie occurrences
# test <- read_delim("./INPUT/CSV/test_df.csv", delim= ",")
# 
# # Select only the "presence" column and convert it in numeric
# myResp_test <- as.numeric(test[["presence"]])
# 
# # Get corresponding XY coordinates
# myRespXY_test <- test[, c('x', 'y')]
# 
