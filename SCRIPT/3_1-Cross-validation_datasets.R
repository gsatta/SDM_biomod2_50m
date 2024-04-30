################################################################################

#                         Cross Validation Dataset

################################################################################

# Load the necessary packages
library(sf); library(blockCV) 

# k-fold selection
cv_k <- bm_CrossValidation_kfold(bm.format = myBiomodData,
                                 nb.rep = 1,
                                 k = 5)

head(cv_k, 2, table)

# # block selection
# cv_b <- bm_CrossValidation_block(bm.format = myBiomodData)
# 
# head(cv_b, 2, table)


# Data <- read_sf("./INPUT/VECTOR/p-psa_adj_unique_points.gpkg")
# 
# 
# scv <- cv_spatial(
#   x = Data,
#   r = myExpl,
#   k = 10, # number of folds
#   hexagon = T,
#   flat_top = T,
#   size = 2000, # size of the blocks in metres
#   selection = "random", # random blocks-to-fold
#   iteration = 50, # find evenly dispersed folds
#   progress = FALSE, # turn off progress bar
#   biomod2 = TRUE, # also create folds for biomod2
#   raster_colors = terrain.colors(10, rev = TRUE), # options from cv_plot for a better colour contrast
#   plot = T
# ) 
# 
# cv_table <- scv$biomod_table
# 
# # Nomi delle PA e delle run
# pa_names <- c("PA1", "PA2", "PA3")
# run_names <- paste0("RUN", 1:10)
# 
# # Creazione di tutte le combinazioni possibili di PA e run
# combined_names <- expand.grid(pa_names, run_names)
# 
# # Aggiunta del prefisso "PA" alle PA
# combined_names$Var1 <- paste0(combined_names$Var1, "_")
# 
# # Concatenazione dei nomi PA e run
# combined_names$combined <- paste0(combined_names$Var1, combined_names$Var2)
# 
# # Rimozione delle colonne non necessarie
# combined_names <- combined_names$combined
# 
# # Stampa dei nomi delle colonne ripetuti per 3 volte
# replicated_names <- rep(combined_names, each = 3)
# 
# # Stampa dei nomi delle colonne ripetuti per 3 volte
# print(replicated_names)
# 
# 
