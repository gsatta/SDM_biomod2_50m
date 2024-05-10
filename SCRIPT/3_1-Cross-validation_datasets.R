################################################################################

#                         Cross Validation Dataset

################################################################################

# Load the necessary packages
library(sf); library(blockCV); library(biomod2); library(caret); library(sf)

set.seed(9999)

# Converti myResp_train in un oggetto sf
myResp_train_sf <- st_as_sf(myResp_train)

# Esegui la cross-validation e crea le suddivisioni
splits <- cv_spatial(
  x = myResp_train_sf,
  column = "presence",
  size = 1000,
  r = myExpl,
  k = 10,
  biomod2 = T,
  report = T,
)

cv_similarity(cv = splits, # the environmental clustering
              x = myResp_train_sf, 
              r = myExpl, 
              progress = FALSE)

cv_spatial_autocor(r = myExpl, 
                   column =  "presence",
                   plot = TRUE)

# Modifica la tabella per avere 3 PA e 10 RUN
biomod_table <- splits$biomod_table

# Stampare la nuova tabella
print(biomod_table)

# Creazione di un nuovo dataframe vuoto per i risultati
new_biomod_table <- data.frame(matrix(ncol = ncol(biomod_table) * 3, nrow = nrow(myBiomodData@PA.table)))
colnames(new_biomod_table) <- paste(rep(paste0("_PA", 1:3), each = ncol(biomod_table)), rep(colnames(biomod_table), 3), sep = "_")

new_biomod_table <- matrix(NA, nrow = nrow(biomod_table), ncol = ncol(biomod_table) * 3)

for (i in 1:ncol(biomod_table)) {
  new_biomod_table[, (3 * i - 2):(3 * i)] <- biomod_table[, i]
}


new_biomod_table

cv_table <- new_biomod_table

cv_table <- bm_CrossValidation(bm.format = myBiomodData,
                           strategy = "kfold",
                           k = 5,
                           nb.rep = 2,
                           do.full.models = TRUE)
 
# Esegui di nuovo la funzione bm_CrossValidation
bm_CrossValidation(
  myBiomodData,
  strategy = "user.defined",
  user.table = cv_table,
  env.var = NULL,
  do.full.models = TRUE
)




































# k-fold selection
# cv_k <- bm_CrossValidation_kfold(bm.format = myBiomodData,
#                                  nb.rep = 1,
#                                  k = 5)
# 
# head(cv_k, 2, table)

# # block selection
cv_b <- biomod2::bm_CrossValidation_block(bm.format = myBiomodData)

head(cv_b, 2, table)


Data <- read_sf("./INPUT/VECTOR/train_data.gpkg")

scv <- cv_spatial(
  x = Data,
  r = myExpl,
  k = 5, # number of folds
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
    col_name <- paste0("_", pseudo_absences[j], "_RUN", i)
    cv_table_user[[col_index]] <- cv_table[, i]
    names(cv_table_user)[col_index] <- col_name
  }
}

# Converti la lista in un data frame
cv_table_user <- as.data.frame(cv_table_user)

# Rimuovere il prefisso "X_" dai nomi delle colonne
colnames(cv_table_user) <- gsub("^X", "", colnames(cv_table_user))

# Ora i nomi delle colonne dovrebbero essere corretti
bm_CrossValidation_user.defined(myBiomodData, cv_table_user)

# Stampare la tabas.matrix()# Stampare la tabella di cross-validation user-defined
print(cv_table_user)






############


# Creazione della classe per biomod.data
setClass("biomod.data",
         slots = c(
           presences = "data.frame",
           background = "data.frame"
         ))

# Ora possiamo definire il metodo per bm_CrossValidation_block
setMethod("bm_CrossValidation_block", signature = c("BIOMOD.formated.data.PA", "numeric"),
          function(bm.format, num_blocks, ...) {
            # Estrai le presenze e le pseudo assenze dai dati di biomod
            presences <- presence(bm.format)
            pseudo_absences <- background(bm.format)
            
            # Unisci presenze e pseudo assenze in un unico dataframe
            all_data <- rbind(presences, pseudo_absences)
            
            # Genera gli indici di riga per la suddivisione in blocchi
            indices <- sample(rep(1:num_blocks, each = nrow(all_data) / num_blocks))
            
            # Inizializza il vettore per memorizzare i risultati della cross-validazione
            cv_results <- vector("list", length = num_blocks)
            
            # Itera su ciascun blocco
            for (i in 1:num_blocks) {
              # Seleziona i dati per il blocco corrente
              test_data <- all_data[indices == i, ]
              train_data <- all_data[indices != i, ]
              
              # Esegui la tua procedura di cross-validazione sui dati di addestramento e test
              # Assicurati di memorizzare i risultati in cv_results[[i]]
              # Esempio: cv_results[[i]] <- tua_funzione_cross_validation(train_data, test_data, ...)
            }
            
            # Restituisci i risultati della cross-validazione
            return(cv_results)
          })

# Ora puoi chiamare la nuova funzione bm_CrossValidation_block con il numero desiderato di blocchi
cv_results <- bm_CrossValidation_block(myBiomodData, num_blocks = 5)
