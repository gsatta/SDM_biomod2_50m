################################################################################

#                             Run Single Models

################################################################################
library(biomod2)

allModels  <- c("FDA", "GAM", "GBM", "GLM", "MAXENT", "RF", "RFd", "XGBOOST")

user.rf <- list('for_all_datasets' = list(type = 'classification', importance = TRUE, 
                nodesize = 10, oob.prox = TRUE, ntree = 250, 
                mtry = 2, maxnodes = 5))
user.maxent <- list('for_all_datasets' = list(visible = TRUE, beta_threshold = 0))

user.XGBOOST <- list('for_all_datasets' = list(objective = "binary:logistic",
                     params = list(max_depth = 5, eta = 0.2, gamma = 1),
                     nrounds = 20, subsample = 0.5))

form.GLM <- bm_MakeFormula(resp.name = myBiomodData@sp.name,
                           expl.var = head(myBiomodData@data.env.var),
                           type = 's_smoother',
                           interaction.level = 0)
user.GLM <- list('for_all_datasets' = list(formula = form.GLM))

form.GAM <- bm_MakeFormula(resp.name = myBiomodData@sp.name,
                           expl.var = head(myBiomodData@data.env.var),
                           type = 's_smoother',
                           interaction.level = 0)
user.GAM <- list('for_all_datasets' = list(algo = 'GAM.mgcv.gam'))

form.GBM <- bm_MakeFormula(resp.name = myBiomodData@sp.name,
                           expl.var = head(myBiomodData@data.env.var),
                           type = 'simple',
                           interaction.level = 0)

user.GBM <- list('for_all_datasets' = list(formula = form.GBM))

form.FDA <- bm_MakeFormula(resp.name = myBiomodData@sp.name,
                           expl.var = head(myBiomodData@data.env.var),
                           type = 's_smoother',
                           interaction.level = 0)
user.FDA <- list('for_all_datasets' = list(formula = form.FDA))


user.val <- list(RF.binary.randomForest.randomForest = user.rf,
                 MAXENT.binary.MAXENT.MAXENT = user.maxent,
                 XGBOOST.binary.xgboost.xgboost = user.XGBOOST,
                 GLM.binary.stats.glm= user.GLM,
                 GAM.binary.mgcv.gam= user.GAM,
                 GBM.binary.gbm.gbm = user.GBM,
                 FDA.binary.mda.fda = user.FDA)

# starting from bigboss parameters
myBiomodOption <- bm_ModelingOptions(data.type = 'binary',
                                     models = allModels,
                                     strategy = "user.defined",
                                     user.base = 'bigboss',
                                     user.val = user.val,
                                     bm.format = myBiomodData)

############################# Run the single models ############################
# single models
myBiomodModelOut <- BIOMOD_Modeling(
  bm.format = myBiomodData,
  OPT.user = myBiomodOption,
  modeling.id = as.character(format(Sys.time(), "%Y%m%d_%H%M_%S")),
  models = allModels,
  models.pa = models.pa,
  CV.strategy = 'kfold',
  CV.nb.rep	= 1,
  CV.k = 5,
  var.import = 3,
  metric.eval = c("ROC", "TSS", "KAPPA", "ACCURACY", "BIAS", "POD", "FAR",
                  "SR", "BOYCE", "MPA"),
  seed.val = 123,
  do.progress = TRUE,
)

# questo è l'ultimo modelllo da usare: 20240703_1154_02


myCalibLines <- get_calib_lines(myBiomodModelOut)
plot(myBiomodData, calib.lines = myCalibLines)


get_formal_data(myBiomodModelOut)

