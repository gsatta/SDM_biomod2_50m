library(biomod2)


n_PA_3 <- length(myResp) 


# Format Data with pseudo - absences: With Cross validation
myBiomodData <- BIOMOD_FormatingData(expl.var = myExpl,
                                     resp.var = myResp,
                                     resp.xy = myRespXY,
                                     resp.name = "Phytophthora",
                                     PA.nb.rep = 3,
                                     PA.nb.absences = c(n_PA_3, 100, 1000),
                                     PA.strategy = 'random',
                                     filter.raster = TRUE,
                                     dir.name = getwd())

# # Format Data with pseudo - absences: random method
# myBiomodData <- BIOMOD_FormatingData(expl.var = myExpl,
#                                      resp.var = myResp_train,
#                                      resp.xy = myRespXY_train,
#                                      eval.expl.var = myResp_test,
#                                      eval.resp.xy =	myRespXY_test,
#                                      resp.name = "Phytophthora",
#                                      PA.nb.rep = 3,
#                                      PA.nb.absences = c(n_PA_3, 100, 1000),
#                                      PA.strategy = 'random',
#                                      filter.raster = TRUE,
#                                      dir.name = getwd())
myBiomodData

plot(myBiomodData)




# # Format Data with pseudo - absences: random method
# myBiomodData_GAM_GLM_MAXENT <- BIOMOD_FormatingData(expl.var = myExpl,
#                                          resp.var = myResp_train,
#                                          resp.xy = myRespXY_train,
#                                          eval.expl.var = myResp_test,
#                                          eval.resp.xy =	myRespXY_test,
#                                          resp.name = "Phytophthora",
#                                          PA.nb.rep = 1,
#                                          PA.nb.absences = c(1000), 
#                                          PA.strategy = 'random',
#                                          filter.raster = TRUE,
#                                          dir.name = getwd())
# 
# 
# # Format Data with pseudo - absences: random method
# myBiomodData_MARS <- BIOMOD_FormatingData(expl.var = myExpl,
#                                              resp.var = myResp_train,
#                                              resp.xy = myRespXY_train,
#                                              eval.expl.var = myResp_test,
#                                              eval.resp.xy =	myRespXY_test,
#                                              resp.name = "Phytophthora",
#                                              PA.nb.rep = 1,
#                                              PA.nb.absences = c(100), 
#                                              PA.strategy = 'random',
#                                              filter.raster = TRUE,
#                                              dir.name = getwd())
# 
# 
# # Format Data with pseudo - absences: random method
# myBiomodData_CTA_RF_GN_FDA <- BIOMOD_FormatingData(expl.var = myExpl,
#                                           resp.var = myResp_train,
#                                           resp.xy = myRespXY_train,
#                                           eval.expl.var = myResp_test,
#                                           eval.resp.xy =	myRespXY_test,
#                                           resp.name = "Phytophthora",
#                                           PA.nb.rep = 1,
#                                           PA.nb.absences = c(n_PA_3), 
#                                           PA.strategy = 'random',
#                                           filter.raster = TRUE,
#                                           dir.name = getwd())
# 
# 
















