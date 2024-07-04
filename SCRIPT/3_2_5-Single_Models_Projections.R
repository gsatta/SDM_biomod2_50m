################################################################################

#                            Single Models Projection

################################################################################
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


xxx <- as.data.frame(myBiomodModelOut@models.computed)

# Usa sub con espressioni regolari per estrarre l'ultima parte dopo l'ultimo underscore
last_parts <- sub(".*_", "", models)

# Aggiungi una nuova colonna al data frame
xxx$models <- last_parts

# Visualizza il risultato
print(xxx)