################################################################################

#                                  Run Single Models

################################################################################

#################### Prepare ascii data for Maxent #############################
{
# Percorso al file maxent.jar
path_to_maxent.jar <- file.path(getwd(), "maxent.jar")

# Cartella dei file ASCII
ascii_folder <- "./INPUT/RASTER/ASCII_standard"

# Elimina la cartella temporanea e tutti i suoi contenuti
unlink("./INPUT/RASTER/maxent_background_data",
       recursive = TRUE)

# Estrai i nomi dei layer senza estensione
layer_names <- gsub("\\.asc$", "", names(myExpl))

# Lista dei file .asc che corrispondono ai nomi dei layer
selected_files <- list.files(ascii_folder, pattern = "\\.asc$")[basename(list.files(ascii_folder, pattern = "\\.asc$")) %in% paste0(layer_names, ".asc")]

# Crea la cartella per i file di background di Maxent
dir.create("./INPUT/RASTER/maxent_background_data")

# Controlla se la directory esiste, altrimenti creala
maxent_background_folder <- "./INPUT/RASTER/maxent_background_data"
if (!dir.exists(maxent_background_folder)) {
  dir.create(maxent_background_folder)
}

# Aggiungi il percorso completo dei file
selected_files <- file.path(ascii_folder, selected_files)

# Copia i file selezionati nella nuova cartella sovrascrivendo i file esistenti
file.copy(from = selected_files, 
          to = maxent_background_folder, 
          overwrite = TRUE)

# Imposta il nuovo percorso per i file .asc
maxent.background.dat.dir <- maxent_background_folder
list.files(maxent.background.dat.dir)
}

########################### Modelling options ##################################
# # Set the modelling options
# myBiomodOption <- bm_ModelingOptions(
#   MAXENT = list( path_to_maxent.jar =  path_to_maxent.jar,
#                  background_data_dir = maxent.background.dat.dir,
#                  maximumiterations = 200,
#                  visible = FALSE,
#                  linear = TRUE,
#                  quadratic = TRUE,
#                  product = TRUE,
#                  threshold = TRUE,
#                  hinge = TRUE,
#                  lq2lqptthreshold = 80,
#                  l2lqthreshold = 10,
#                  hingethreshold = 15,
#                  beta_threshold = -1,
#                  beta_categorical = -1,
#                  beta_lqp = -1,
#                  beta_hinge = -1,
#                  defaultprevalence = 0.5),
#   GLM = list(type = 'polynomial', interaction.level = 1),
#   GBM = list(n.trees = 1500),
#   GAM = list(interaction.level = 1, type = 's_smoother', algo = 'GAM_gam'),
#   CTA = NULL,
#   SRE = list(quant = 0.025),
#   FDA = NULL,
#   MARS = list(type = 'polynomial', interaction.level = 1),
#   RF = list(do.classif = FALSE, ntree = 1000, importance = TRUE, nodesize = 10, maxnodes = 5),
#   XGBOOST = list(max.depth = 5, eta = 0.01, nrounds = 1000, objective = "binary:logistic", nthread = 1)
#  )


allModels  <- c("CTA", "FDA", "GAM", "GBM", "GLM", "MARS", "MAXENT", "MAXNET", "RF", "SRE", "XGBOOST")


# bigboss parameters
myBiomodOption <- bm_ModelingOptions(data.type = 'binary',
                            models = allModels,
                            strategy = 'bigboss')


############################# Run the single models ############################
# single models
myBiomodModelOut <- BIOMOD_Modeling(
  bm.format = myBiomodData,
  bm.options = myBiomodOption,
  modeling.id = as.character(format(Sys.time(), "%Y%m%d_%H%M_%S")),
  models = allModels,
  models.pa = models.pa,
  CV.strategy = 'kfold',
  CV.nb.rep	= 2,
  CV.k = 5,
  var.import = 3,
  metric.eval = c("FAR", "SR", "BOYCE"),
  seed.val = 123,
  do.progress = TRUE,
  nb.cpu = 3
)

myCalibLines <- get_calib_lines(myBiomodModelOut)
plot(myBiomodData, calib.lines = myCalibLines)


get_formal_data(myBiomodModelOut)

free(myBiomodModelOut)













