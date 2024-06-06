################################################################################

#                   Ensemble Models response curves

################################################################################

# Represent response curves
resp_models_min <- bm_PlotResponseCurves(bm.out = myBiomodEM, 
                      models.chosen = get_built_models(myBiomodEM)[c(1)],
                      fixed.var = 'min')
resp_models_max <- bm_PlotResponseCurves(bm.out = myBiomodEM, 
                      models.chosen = get_built_models(myBiomodEM)[c(1)],
                      fixed.var = 'max')
resp_models_median <- bm_PlotResponseCurves(bm.out = myBiomodEM, 
                      models.chosen = get_built_models(myBiomodEM)[c(1)],
                      fixed.var = 'median')

resp_models <- bm_PlotResponseCurves(bm.out = myBiomodEM, 
                        models.chosen = get_built_models(myBiomodEM)[c(1)])
                                            

# Get the min, max and median response curve
resp_min <- resp_models_min$tab
resp_max <- resp_models_max$tab
resp_median <- resp_models_median$tab
resp_mean <- resp_models_mean$tab
resp <- resp_models$tab

# Aggiungi le colonne pred.val_min, pred.val_max e pred.val_median
resp_min$pred.val_min <- resp_min$pred.val
resp_max$pred.val_max <- resp_max$pred.val
resp_median$pred.val_median <- resp_median$pred.val
resp_mean$pred.val_mean<- resp_mean$pred.val
resp$pred.val_no <- resp$pred.val

# Rimuovi la colonna pred.val dai data frame originali per evitare duplicati
resp_min <- resp_min[, !(names(resp_min) %in% "pred.val")]
resp_max <- resp_max[, !(names(resp_max) %in% "pred.val")]
resp_median <- resp_median[, !(names(resp_median) %in% "pred.val")]
resp <- resp[, !(names(resp) %in% "pred.val")]

# Unisci i data frame utilizzando id, expl.name e expl.val come chiavi
merged_df <- merge(resp_min, resp_max, by = c("id", "expl.name", "expl.val"))
merged_df <- merge(merged_df, resp_median, by = c("id", "expl.name", "expl.val"))

merged_df$pred.name.x <- NULL
merged_df$pred.name.y <- NULL

merged_df <- merge(merged_df, resp_mean, by = c("id", "expl.name", "expl.val"))
merged_df$pred.name.x <- NULL
merged_df$pred.name.y <- NULL
merged_df <- merge(merged_df, resp, by = c("id", "expl.name", "expl.val")) 

# Elimina le altre colonne dupplicate
merged_df$pred.name.x <- NULL
merged_df$pred.name.y <- NULL
merged_df$pred.val <- NULL




# Carica i pacchetti necessari
library(ggplot2)
library(dplyr)




# Crea un grafico per ogni 'expl.name'
p <- ggplot(merged_df, aes(x = expl.val)) +
  geom_ribbon(aes(ymin = pred.val_min, ymax = pred.val_max), fill = "lightgrey", alpha = 0.5) +
  geom_smooth(aes(y = pred.val_no.x), method = "loess", color = "blue") +
  facet_wrap(~ expl.name, scales = "free_x") +
  labs(title = "vvvvv",
       x = "Predicted value",
       y = "Probability") +
  theme_minimal()

# Visualizza il grafico
print(p)



# Crea un grafico per ogni 'expl.name'
p_mean <- ggplot(merged_df, aes(x = expl.val)) +
  geom_ribbon(aes(ymin = pred.val_min, ymax = pred.val_max), fill = "lightgrey", alpha = 0.5) +
  geom_smooth(aes(y = pred.val_mean), method = "loess", color = "blue") +
  facet_wrap(~ expl.name, scales = "free_x") +
  labs(title = "vvvvv",
       x = "Predicted value",
       y = "Probability") +
  theme_minimal()

# Visualizza il grafico
print(p_mean)

























require(foreach)
require(confintr)

tmp = resp_models$tab
tmp.split = split(tmp, list(tmp$expl.name, tmp$id))
tmp.mean = foreach(tabi = tmp.split, .combine = "rbind") %do%
  {
    res = unique(tabi[, c("id", "expl.name", "expl.val")])
    res$pred.val = mean(tabi$pred.val)
    res$conf.inf = ci_mean(tabi$pred.val)$interval[1]
    res$conf.sup = ci_mean(tabi$pred.val)$interval[2]
    return(res)
  }
head(tmp.mean)

ggplot(tmp.mean, aes_string(x = "expl.val", y = "pred.val")) + 
  geom_ribbon(aes(ymin = conf.inf, ymax = conf.sup), fill = "grey60") +
  geom_line() + 
  facet_wrap("expl.name", scales = "free_x") + 
  xlab("") + 
  ylab("") + 
  ylim(0, 1) +
  theme(legend.title = element_blank(), 
        legend.key = element_rect(fill = "white"), legend.position = "bottom", 
        axis.text.x = element_text(angle = 45, hjust = 1))








library(foreach)

# Supponendo che ci sia una funzione ci_mean che calcola gli intervalli di confidenza
ci_mean <- function(x) {
  # Sostituisci questa funzione con quella effettiva
  # Qui viene calcolato un intervallo di confidenza fittizio
  mean_x <- mean(x, na.rm = TRUE)
  sd_x <- sd(x, na.rm = TRUE)
  n <- length(x[!is.na(x)])
  error <- qt(0.975, df = n-1) * sd_x / sqrt(n)
  return(list(interval = c(mean_x - error, mean_x + error)))
}

# Dividi i dati in base a expl.name e id
tmp.split = split(tmp, list(tmp$expl.name, tmp$id))

# Calcola la media e gli intervalli di confidenza
tmp.mean = foreach(tabi = tmp.split, .combine = "rbind") %do% {
  if(any(is.na(tabi$pred.val))) {
    warning("Valori mancanti trovati in pred.val. Saranno ignorati nel calcolo.")
    tabi <- tabi[!is.na(tabi$pred.val), ]
  }
  
  if(nrow(tabi) == 0) {
    warning("Nessun dato valido per il gruppo corrente.")
    next
  }
  
  res = unique(tabi[, c("id", "expl.name", "expl.val")])
  res$pred.val = mean(tabi$pred.val, na.rm = TRUE)
  ci = ci_mean(tabi$pred.val)
  res$conf.inf = ci$interval[1]
  res$conf.sup = ci$interval[2]
  return(res)
}

# Visualizza i primi 50 avvertimenti se ce ne sono
if (length(warnings()) > 0) {
  print(warnings()[1:50])
}

tmp.mean
