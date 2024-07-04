################################################################################

#                        Prepare Data and Parameters

################################################################################

# Load the necessary packages
library(biomod2)


n_PA_3 <- length(myResp_train) 


# Format Data with pseudo - absences: random method
myBiomodData <- BIOMOD_FormatingData(expl.var = myExpl,
                                     resp.var = myResp_train,
                                     eval.resp.var = myResp_test,
                                     resp.name = "Phytophthora",
                                     PA.nb.rep = 20,
                                     PA.nb.absences =  c(rep(n_PA_3, 10), rep(1000, 10)),
                                     PA.strategy = 'random',
                                     filter.raster = TRUE,
                                     dir.name = getwd(),
                                     seed.val = 1234)
myBiomodData

# plot(myBiomodData)

# Definisci i nomi delle colonne disponibili all'interno di bm.format@PA.table
PA_used <- colnames(myBiomodData@PA.table)

# Definisci i vettori per ciascun modello che indicano quali colonne di pseudo-assenza utilizzare
{
  PA1_10 <- paste0("PA", 1:10)
  PA11_20 <- paste0("PA", 11:20)
  
  # CTA <- PA1_10
  FDA <- PA1_10
  RF <- PA1_10
  RFd <- PA1_10
  XGBOOST <- PA1_10
  GLM <- PA11_20
  GAM <- PA11_20
  MAXENT <- PA11_20
  MAXNET <- PA11_20
  GBM <- PA11_20
}

# Costruisci la lista models.pa contenente questi vettori per ciascun modello
models.pa <- list(FDA = FDA, GBM = GBM, RF = RF, RFd = RFd, XGBOOST = XGBOOST, 
                  GLM = GLM, GAM = GAM, MAXENT = MAXENT)



