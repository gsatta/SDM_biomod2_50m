# Run modellization
path_to_maxent.jar <- file.path(getwd(), "maxent.jar")

maxent.background.dat.dir <- "./INPUT/RASTER/ASCII"

list.files(maxent.background.dat.dir, pattern = "\\.asc$")

myBiomodOption <- BIOMOD_ModelingOptions(
  MAXENT = list( path_to_maxent.jar =  path_to_maxent.jar,
                 background_data_dir = maxent.background.dat.dir,
                 maximumiterations = 200,
                 visible = FALSE,
                 linear = TRUE,
                 quadratic = TRUE,
                 product = TRUE,
                 threshold = TRUE,
                 hinge = TRUE,
                 lq2lqptthreshold = 80,
                 l2lqthreshold = 10,
                 hingethreshold = 15,
                 beta_threshold = -1,
                 beta_categorical = -1,
                 beta_lqp = -1,
                 beta_hinge = -1,
                 defaultprevalence = 0.5),
  # GLM = list(type = 'polynomial', interaction.level = 1),
  # GBM = list(n.trees = 1500),
  GAM = list(interaction.level = 1, type = 's_smoother', algo = 'GAM_mgcv'),
  # CTA = NULL,
  # # ANN = list(NbCV = 20, size = NULL, decay = NULL),
  # SRE = list(quant = 0.025),
  # FDA = NULL,
  # MARS = list(type = 'polynomial', interaction.level = 1),
  RF = list(do.classif = FALSE, ntree = 1000, importance = TRUE, nodesize = 10, maxnodes = 5)
  # XGBOOST = list(max.depth = 5, eta = 0.01, nrounds = 1000, objective = "binary:logistic", nthread = 1)
  )

# single models
myBiomodModelOut <- BIOMOD_Modeling(
  bm.format = myBiomodData,
  bm.options = myBiomodOption,
  modeling.id = as.character(format(Sys.time(), "%Y%m%d_%H%M_%S")),
  models = c('GLM', 'GBM', 'GAM', 'CTA', 'SRE', 'FDA', 'MARS', 'RF', 'MAXNET', 'MAXENT'),
  CV.strategy = 'random',
  CV.nb.rep = 1,
  CV.perc = 0.8,
  var.import = 3,
  metric.eval = c("TSS", "ROC"),
  # metric.eval = c("ROC"),
  # nb.cpu = 4,
  seed.val = 123,
  do.progress = TRUE
)

# Get evaluation scores & variables importance
models_score <- get_evaluations(myBiomodModelOut)
var_imp <- get_variables_importance(myBiomodModelOut)


#######################
## models predictions
pred.val <- get_predictions(myBiomodModelOut) 
dim(pred.val)

## formale response variable 
form.dat <- get_formal_data(myBiomodModelOut, 'resp.var')
length(form.dat)


calib.lines <- get_calib_lines(myBiomodModelOut, 
                               PA = NULL, 
                               as.data.frame = TRUE)
calib.lines <- na.omit(calib.lines)

dim(calib.lines)

dimnames(calib.lines)


## get all combination of model, cv run, pa data
model.comb <- expand.grid(
  mod = unique(pred.val$algo),
  cv = unique(pred.val$run),
  pa = unique(pred.val$PA),
  stringsAsFactors = FALSE
)

## compute all the roc curves
mod.roc <- lapply(
  1:nrow(model.comb),
  function(i) {
    mod <- model.comb$mod[i]
    cv <- model.comb$cv[i]
    pa <- model.comb$pa[i]
    
    eval.lines <- !calib.lines$calib.lines & !calib.lines$calib.lines
    eval.lines <- na.omit(eval.lines)
    
    resp <- form.dat[eval.lines]
    pred <- pred.val[eval.lines, which(colnames(pred.val) == mod)]
    
    pROC::roc(resp, pred / 1000)
  }
)

library(purrr)

library(purrr)

# Definisci una funzione per calcolare l'AUC per una specifica combinazione di modello, cv run e dati PA
calcola_auc <- function(mod, cv, pa) {
  # Filtra i dati in base alla combinazione specifica
  subset_data <- subset(pred.val, algo == mod & run == cv & PA == pa)
  
  # Calcola la curva ROC
  roc_data <- multiclass.roc(subset_data$points, subset_data$pred)
  
  # Calcola l'AUC
  auc_value <- auc(roc_data)
  
  return(auc_value)
}

# Applica la funzione a tutte le combinazioni di modello, cv run e dati PA
auc_values <- pmap_dbl(model.comb, calcola_auc)

# Crea un dataframe con i risultati
result_df <- cbind(model.comb, AUC = auc_values)

# Stampa il dataframe
print(result_df)


##################################

# Creazione del dataframe con le metriche del modello
model_metrics <- as.data.frame(myBiomodModelOut@models.evaluation@val)

# Visualizzazione del dataframe
print(model_metrics)

dim(models_score)
dimnames(models_score)

# Represent evaluation scores & variables importance
bm_PlotEvalMean(bm.out = myBiomodModelOut, 
                group.by = c('algo'), 
                dataset = "calibration", 
                do.plot = TRUE, 
                xlim = c(0, 1),  # Modifica i limiti dell'asse x
                ylim = c(0, 1),  # Modifica i limiti dell'asse y
                main = "Mean Evaluation Scores")  # Modifica il titolo del grafico

bm_PlotEvalMean(bm.out = myBiomodModelOut, 
                group.by = c('PA'), 
                dataset = "calibration", 
                do.plot = TRUE, 
                xlim = c(0, 1),  # Modifica i limiti dell'asse x
                ylim = c(0, 1),  # Modifica i limiti dell'asse y
                main = "Mean Evaluation Scores")  # Modifica il titolo del grafico

bm_PlotEvalBoxplot(bm.out = myBiomodModelOut, group.by = c('algo', 'algo'))
bm_PlotEvalBoxplot(bm.out = myBiomodModelOut, group.by = c('algo', 'run'))
bm_PlotVarImpBoxplot(bm.out = myBiomodModelOut, group.by = c('expl.var', 'algo', 'algo'))
bm_PlotVarImpBoxplot(bm.out = myBiomodModelOut, group.by = c('expl.var', 'algo', 'run'))
bm_PlotVarImpBoxplot(bm.out = myBiomodModelOut, group.by = c('algo', 'expl.var', 'run'))

# Represent response curves
bm_PlotResponseCurves(bm.out = myBiomodModelOut, 
                      models.chosen = get_built_models(myBiomodModelOut)[c(11:20, 11:20)],
                      fixed.var = 'median')
bm_PlotResponseCurves(bm.out = myBiomodModelOut, 
                      models.chosen = get_built_models(myBiomodModelOut)[c(1:3, 12:14)],
                      fixed.var = 'min')
bm_PlotResponseCurves(bm.out = myBiomodModelOut, 
                      models.chosen = get_built_models(myBiomodModelOut)[3],
                      fixed.var = 'median',
                      do.bivariate = TRUE)

# Project single models
myBiomodProj <- BIOMOD_Projection(bm.mod = myBiomodModelOut,
                                  proj.name = 'Current',
                                  new.env = myExpl,
                                  models.chosen = 'all',
                                  metric.binary = 'all',
                                  metric.filter = 'all',
                                  build.clamping.mask = TRUE,
                                  output.format = ".tif")
myBiomodProj
plot(myBiomodProj)

EN_models_score <- get_evaluations(myBiomodModelOut)

# laod the packages
library(precrec)
library(ggplot2) # for plotting the curves

# Estrai le probabilità predette per ogni modello
# Ottieni le probabilità predette per ogni modello
predicted_probs <- get_predictions(myBiomodModelOut)


# Estrai i nomi dei modelli e i rispettivi valori AUC
model_names <- models_score$full.name
roc_values <- models_score$validation

# Calcola l'AUC per la curva ROC
auc_value <- auc(roc_values)

# Crea un grafico a barre per visualizzare l'AUC per ciascun modello
barplot(auc_values, names.arg = model_names, main = "AUC per modello", xlab = "Modello", ylab = "AUC", col = "skyblue", las = 2)




