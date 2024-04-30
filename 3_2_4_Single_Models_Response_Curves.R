################################################################################

#                          Single Models Response Curves

################################################################################

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



response_curves <- myBiomodModelOut@models.prediction

response_curves

get_formal_model(myBiomodModelOut)

# Carica il pacchetto biomod2
library(biomod2)

# Carica il modello Biomod
load("percorso/al/modello/myBiomodModelOut.RData")

# Utilizza la funzione bm_PlotResponseCurves per estrarre i dati delle curve di risposta
response_data <- bm_PlotResponseCurves(bm.out = myBiomodModelOut, do.plot = FALSE)

# Dai un'occhiata alla struttura dei dati estratti
str(response_data)

library(ggplot2)

# Estrai i dati delle curve di risposta per un modello specifico
response_curves <- response_data$tab
# model_name <- "Phytophthora_PA3_RUN10_GBM"
# model_data <- response_curves[response_curves$pred.name == model_name, ]
# 
# # Plot delle curve di risposta per il modello specifico
# ggplot(model_data, aes(x = expl.val, y = pred.val, color = as.factor(expl.name))) +
#   geom_line() +
#   facet_wrap(~ expl.name, scales = "free") +
#   labs(title = paste("Response curves for", model_name))

library(dplyr)
library(ggplot2)

library(gridExtra)

# Lista dei nomi dei modelli unici
model_names <- myBiomodModelOut@models.computed

library(dplyr)
library(ggplot2)

# Definisci una funzione per calcolare la mediana delle curve per ogni modello
calculate_median_curve <- function(model_name, data) {
  # Filtra i dati per il modello specificato
  model_data <- data %>%
    filter(pred.name == model_name)
  
  # Calcola la mediana delle curve per ogni variabile
  median_curve <- model_data %>%
    group_by(expl.val) %>%
    summarise(median_pred_val = median(pred.val))
  
  return(median_curve)
}

# Inizializza una lista per memorizzare i dataframe delle curve mediane
median_curves <- list()

# Calcola la curva mediana per ciascun modello
for (model_name in model_names) {
  model_median_curve <- calculate_median_curve(model_name, response_data$tab)
  median_curves[[model_name]] <- model_median_curve
}

# Crea un grafico per ogni modello
for (model_name in model_names) {
  model_median_curve <- median_curves[[model_name]]
  
  # Visualizza la curva per il modello corrente
  p <- ggplot(model_median_curve, aes(x = expl.val, y = median_pred_val)) +
    geom_line(color = "blue") +
    labs(title = paste("Curva mediana per il modello", model_name),
         y = "Mediana dei valori predetti") +
    theme_minimal() +
    facet_wrap(~ expl.val, scales = "free") +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  print(p)
}


