# solo RF e xgboost

#####################################
# Definisci i vettori per ciascun modello che indicano quali colonne di pseudo-assenza utilizzare
{
  RF <- c("PA1")
  # XGBOOST <- c("PA1")
}

# Costruisci la lista models.pa contenente questi vettori per ciascun modello
models.pa <- list(RF = RF)
                   # XGBOOST = XGBOOST)

allModels  <- c("RF")
                # "XGBOOST")

# user.rf <- list('_allData_allRun' = list(type = 'classification', importance = TRUE, 
#                                          mtry = 2, oob.prox = T))
# 
# user.XGBOOST <- list('_allData_allRun' = list(
#   objective = "binary:logistic",
#   params =list(max_depth = 10, eta = 0.3),
#   nrounds = 10, subsample = 0.7,
#   missing = -9999))

# user.val_0 <- list(RF.binary.randomForest.randomForest = user.rf,
#                  XGBOOST.binary.xgboost.xgboost = user.XGBOOST)

# bigboss parameters
myBiomodOption <- bm_ModelingOptions(data.type = 'binary',
                                     models = allModels,
                                     strategy = "bigboss",
                                     calib.lines = cv.k,
                                     bm.format = myBiomodData)

# tune parameters for Random Forest model
tuned.rf <- bm_Tuning(model = 'RF',
                      tuning.fun = 'rf', ## see in ModelsTable
                      do.formula = TRUE,
                      bm.options = myBiomodOption@options$RF.binary.randomForest.randomForest,
                      bm.format = myBiomodData,
                      calib.lines = cv.k)

# form.GLM <- bm_MakeFormula(resp.name = myBiomodData@sp.name,
#                            expl.var = head(myBiomodData@data.env.var),
#                            type = 'simple',
#                            interaction.level = 0)
# 
# user.GLM <- list('_allData_RUN1' = list(formula = form.GLM),
#                  '_allData_RUN2' = list(formula = form.GLM))

# # tune parameters for GAM (from mgcv package) model
# tuned.gam <- bm_Tuning(model = 'GAM',
#                        tuning.fun = 'gam', ## see in ModelsTable
#                        do.formula = TRUE,
#                        do.stepAIC = TRUE,
#                        bm.options = myBiomodOption@options$GAM.binary.mgcv.gam,
#                        bm.format = myBiomodData)

## Gather in one list
user.val_1 <- list( RF.binary.randomForest.randomForest = tuned.rf)
                  # GAM.binary.mgcv.gam= tuned.gam)

myOpt <- bm_ModelingOptions(data.type = 'binary',
                            models = allModels,
                            strategy = "bigboss",
                            # strategy = "user.defined",
                            # user.val = user.val_1,
                            # user.base = "bigboss",
                            bm.format = myBiomodData)

# single models
myBiomodModelOut <- BIOMOD_Modeling(
  bm.format = myBiomodData,
  OPT.user = myBiomodOption,
  modeling.id = as.character(format(Sys.time(), "%Y%m%d_%H%M_%S")),
  models = allModels,
  models.pa = models.pa,
  CV.strategy = 'user.defined',
  CV.user.table = cv.k,
  CV.nb.rep	= 2,
  CV.k = 5,
  var.import = 3,
  metric.eval = c("FAR", "SR", "BOYCE", "ROC", "TSS", "KAPPA", "ACCURACY", "BIAS"),
  seed.val = 123,
  do.progress = TRUE,
)
