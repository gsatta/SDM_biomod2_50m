################################################################################

#                             Variable selection

##############################################################################
# allModels  <- c("CTA", "FDA", "GAM", "GBM", "GLM", "MARS", "MAXENT", "MAXNET", "RF", "RFdf", XGBOOST", "SRE")
library(terra); library(biomod2)

myResp_train <- vect("./INPUT/VECTOR/train_data.gpkg")

myResp_test <- vect("./INPUT/VECTOR/test_data.gpkg")

# Load the environmental raster  at 50 m of spatial resolution
myExpl_0_0 <- rast("./INPUT/RASTER/environmental_50m.tif")

#   Variables      VIF
# 1      roads 1.143632
# 2     rivers 1.388255
# 3     aspect 1.073247
# 4        fla 1.276774
# 5      slope 2.224088
# 6        tpi 1.264534
# 7        twi 2.245807
# 8        soc 2.884837
# 9        bkd 1.422554
# 10     BIO03 2.221509
# 11     BIO06 1.958931
# 12     BIO15 1.421190
# 13  ndwi2021 1.307304
# 14     dndvi 1.257369
# 15        wo 1.618243

# Nomi delle variabili non collineari
non_collinear_vars <- c("roads", "rivers", "aspect", "fla", "slope", "tpi",
                        "twi", "soc", "bkd", "BIO03", "BIO06",
                        "BIO15", "ndwi2021", "dndvi", "wo")
  
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
  GLM <- c("PA3")
  GAM <- c("PA3")
  MAXENT <- c("PA3")
  MAXNET <- c("PA3")
  GBM <- c("PA3")
  
}

# Costruisci la lista models.pa contenente questi vettori per ciascun modello
models.pa <- list(GBM = GBM, RF = RF, XGBOOST = XGBOOST, 
                  MARS = MARS, 
                  GLM = GLM, GAM = GAM, MAXENT = MAXENT )


allModels  <- c("GAM", "GBM", "GLM", "MARS", "MAXENT", "RF", "XGBOOST")

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

myBiomodModelOut
# Single models

(bm_out_file <- load("./Phytophthora/Phytophthora.20240717_1526_17.models.out"))

myBiomodModelOut <- get(bm_out_file)
rm(list = c(bm_out_file, 'bm_out_file'))


var_imp <- get_variables_importance(myBiomodModelOut) # 20240612_1502_30

# bm_PlotVarImpBoxplot(bm.out = myBiomodModelOut, group.by = c('expl.var', 'algo', 'algo'))
# bm_PlotVarImpBoxplot(bm.out = myBiomodModelOut, group.by = c('expl.var', 'algo', 'run'))
# bm_PlotVarImpBoxplot(bm.out = myBiomodModelOut, group.by = c('algo', 'expl.var', 'run'))

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


