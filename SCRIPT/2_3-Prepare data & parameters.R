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
                                     PA.nb.rep = 3,
                                     PA.nb.absences = c(n_PA_3, 100, 1000),
                                     PA.strategy = 'random',
                                     filter.raster = TRUE,
                                     dir.name = getwd())
myBiomodData

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



