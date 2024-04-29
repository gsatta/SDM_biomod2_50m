library(biomod2)


n_PA_3 <- length(myResp_train) 


# Format Data with pseudo - absences: random method
myBiomodData <- BIOMOD_FormatingData(expl.var = myExpl,
                                     resp.var = myResp_train,
                                     resp.xy = myRespXY_train,
                                     eval.expl.var = myResp_test,
                                     eval.resp.xy =	myRespXY_test,
                                     resp.name = "Phytophthora",
                                     PA.nb.rep = 3,
                                     PA.nb.absences = c(n_PA_3, 100, 1000),
                                     PA.strategy = 'random',
                                     filter.raster = TRUE,
                                     dir.name = getwd())
myBiomodData

plot(myBiomodData)