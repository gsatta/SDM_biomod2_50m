################################################################################

#                         Cross Validation Dataset

################################################################################

# Load the necessary packages
library(sf); library(blockCV) 

# k-fold selection
# cv_k <- bm_CrossValidation_kfold(bm.format = myBiomodData,
#                                  nb.rep = 1,
#                                  k = 5)
# 
# head(cv_k, 2, table)

# # block selection
# cv_b <- bm_CrossValidation_block(bm.format = myBiomodData)
# 
# head(cv_b, 2, table)


Data <- read_sf("./INPUT/VECTOR/train_data.gpkg")


scv <- cv_spatial(
  x = Data,
  r = myExpl,
  k = 10, # number of folds
  hexagon = T,
  flat_top = T,
  size = 2000, # size of the blocks in metres
  selection = "random", # random blocks-to-fold
  iteration = 50, # find evenly dispersed folds
  progress = FALSE, # turn off progress bar
  biomod2 = TRUE, # also create folds for biomod2
  raster_colors = terrain.colors(10, rev = TRUE),
  plot = T
)

# Utilizzo della tabella di cross-validation originale
cv_table <- scv$biomod_table

# Creazione della tabella di cross-validation user-defined
cv_table_user <- vector("list", length = 30)
pseudo_absences <- c("PA1", "PA2", "PA3")

for (i in 1:10) {
  for (j in 1:length(pseudo_absences)) {
    col_index <- (i-1)*length(pseudo_absences) + j
    col_name <- paste0(pseudo_absences[j], "_RUN", i)
    cv_table_user[[col_index]] <- cv_table[, i]
    names(cv_table_user)[col_index] <- col_name
  }
}

# Convertire la lista in un data frame
cv_table_user <- as.matrix(cv_table_user)

# Stampare la tabas.matrix()# Stampare la tabella di cross-validation user-defined
print(cv_table_user)
