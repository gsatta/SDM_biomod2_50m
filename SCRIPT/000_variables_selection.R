################################################################################

#                             Variable selection

##############################################################################allModels  <- c("CTA", "FDA", "GAM", "GBM", "GLM", "MARS", "MAXENT", "MAXNET", "RF", "XGBOOST", "SRE")
library(terra); library(biomod2)

myResp_train <- vect("./INPUT/VECTOR/train_data.gpkg")

myResp_test <- vect("./INPUT/VECTOR/test_data.gpkg")

# Load the environmental raster  at 50 m of spatial resolution
myExpl_0_0 <- rast("./INPUT/RASTER/environmental_50m.tif")

# Select only the non collinear variables
# [1] "bkd"    "clay"   "sand"   "silt"   "soc"    "twi"    "tpi"    "fla"    "rivers" "roads" 
# [11] "green"  "slope"  "BIO03"  "BIO04"  "BIO06"  "aspect"
# > 
  
# Nomi delle variabili non collineari
non_collinear_vars <- c("bkd", "clay", "sand", "silt", "soc", 
                        "twi", "tpi", "fla", "rivers", "roads", 
                        "green", "slope", "BIO3", "BIO4", "BIO06", 
                        "aspect")
  
# Seleziona solo le variabili non collineari
myExpl_0 <- myExpl_0_0[[non_collinear_vars]]
  
# Visualizza il risultato
print(myExpl_0)

n_PA_3 <- length(myResp_train) 

# Format Data with pseudo - absences: random method
myBiomodData <- BIOMOD_FormatingData(expl.var = myExpl_0,
                                     resp.var = myResp_train,
                                     eval.resp.var = myResp_test,
                                     resp.name = "Phytophthora",
                                     PA.nb.rep = 3,
                                     PA.nb.absences = c(n_PA_3, 100, 1000),
                                     PA.strategy = 'random',
                                     filter.raster = TRUE,
                                     dir.name = getwd())
myBiomodData

# # k-fold selection
# cv.k <- bm_CrossValidation(bm.format = myBiomodData,
#                            strategy = "kfold",
#                            nb.rep = 2,
#                            k = 5)
# plot(myBiomodData)

# Definisci i nomi delle colonne disponibili all'interno di bm.format@PA.table
PA_used <- colnames(myBiomodData@PA.table)

# Definisci i vettori per ciascun modello che indicano quali colonne di pseudo-assenza utilizzare
{
  CTA <- c("PA1")
  FDA <- c("PA1")
  RF <- c("PA1")
  XGBOOST <- c("PA1")
  MARS <- c("PA2")
  SRE <- c("PA2")
  GLM <- c("PA3")
  GAM <- c("PA3")
  MAXENT <- c("PA3")
  MAXNET <- c("PA3")
  GBM <- c("PA3")
  
}

# Costruisci la lista models.pa contenente questi vettori per ciascun modello
models.pa <- list(CTA = CTA, FDA = FDA, GBM = GBM, RF = RF, XGBOOST = XGBOOST, 
                  MARS = MARS, 
                  GLM = GLM, GAM = GAM, MAXENT = MAXENT, MAXNET = MAXNET  )
allModels  <- c("CTA", "FDA", "GAM", "GBM", "GLM", "MARS", "MAXENT", "MAXNET", "RF", "XGBOOST", "SRE")

user.rf <- list('_allData_allRun' = list(type = 'classification', importance = TRUE, 
                                         nodesize = 1, oob.prox = T, 
                                         mtry = 2))

user.maxent <- list('_allData_allRun' = list(visible = TRUE))

user.XGBOOST <- list('_allData_allRun' = list(objective = "binary:logistic",
                                              params =list(max_depth = 5, eta = 0.2, gamma = 1 ),
                                              nrounds = 20, subsample = 0.5,
                                              missing = -9999))

form.GLM <- bm_MakeFormula(resp.name = myBiomodData@sp.name,
                           expl.var = head(myBiomodData@data.env.var),
                           type = 's_smoother',
                           interaction.level = 0)

user.GLM <- list('_allData_allRun' = list(formula = form.GLM))


form.GAM <- bm_MakeFormula(resp.name = myBiomodData@sp.name,
                           expl.var = head(myBiomodData@data.env.var),
                           type = 's_smoother',
                           interaction.level = 0)

user.GAM <- list('_allData_allRun' = list(formula = form.GAM))

user.val <- list(RF.binary.randomForest.randomForest = user.rf,
                 MAXENT.binary.MAXENT.MAXENT = user.maxent,
                 XGBOOST.binary.xgboost.xgboost = user.XGBOOST,
                 GLM.binary.stats.glm= user.GLM,
                 GAM.binary.mgcv.gam= user.GAM)

# bigboss parameters
myBiomodOption <- bm_ModelingOptions(data.type = 'binary',
                                     models = allModels,
                                     strategy = "user.defined",
                                     user.base = 'bigboss',
                                     user.val = user.val)

############################# Run the single models ############################
# single models 20240528_1850_52
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
  metric.eval = c("FAR", "SR", "BOYCE", "ROC", "TSS", "KAPPA", "ACCURACY", "BIAS"),
  seed.val = 123,
  do.progress = TRUE,
)

# Get variables importance

var_imp <- get_variables_importance(myBiomodModelOut) # 20240528_1850_52

bm_PlotVarImpBoxplot(bm.out = myBiomodModelOut, group.by = c('expl.var', 'algo', 'algo'))
bm_PlotVarImpBoxplot(bm.out = myBiomodModelOut, group.by = c('expl.var', 'algo', 'run'))
bm_PlotVarImpBoxplot(bm.out = myBiomodModelOut, group.by = c('algo', 'expl.var', 'run'))

library(dplyr)

# Raggruppa i dati per expl.var e calcola la media di var.imp
ranked_vars <- var_imp %>%
  group_by(expl.var) %>%
  summarize(mean_var_imp = mean(var.imp)) %>%
  # Ordina i gruppi in base alla media di var.imp in ordine decrescente
  arrange(desc(mean_var_imp)) %>%
  # Aggiungi una colonna con il rank
  mutate(rank = row_number())

# Stampare i risultati
print(ranked_vars)


