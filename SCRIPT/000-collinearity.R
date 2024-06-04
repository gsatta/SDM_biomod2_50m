################################################################################

#                        Variables Selection

################################################################################

library(readr); library(usdm); library(corrplot)

# Imposta il seed per rendere le analisi ripetibili
set.seed(999)


environmental <- rast("./INPUT/RASTER/environmental_50m.tiff")

# Estrai le coordinate delle celle e i valori di tutti i layer
environmental_df <- as.data.frame(environmental, xy = FALSE, na.rm=TRUE)

zs <- environmental_df

zs$occ <- NULL

zs$sand <- NULL

zs$silt <- NULL

zs$clay <- NULL

# zs$texture <- NULL

# Converti il dataframe in un dataframe standard
zs_standard <- as.data.frame(zs)

options(scipen = 999)

# # Calcola la media delle variabili selezionate
# media_zs <- colMeans(zs, na.rm = TRUE)
# # Calcola la deviazione standard delle variabili selezionate
# dev_standard_zs <- apply(zs, 2, sd, na.rm = TRUE)
# # Calcola la varianza delle variabili selezionate
# var_zs <- apply(zs, 2, var, na.rm = TRUE)
# 
# # Crea la tabella con le deviazioni standard e le varianze
# dev_var_table <- data.frame(
#   Variabile = names(dev_standard_zs),
#   sd = dev_standard_zs,
#   var= var_zs
# )
# 
# # Stampare la tabella
# dev_var_table
# 
# var_seleced_sd <- dev_var_table[dev_var_table$sd > 10, ]
# 
# var_seleced_sd$Variabile
# 
# # Seleziona le varibili con sd > 10
# zs_selected <- subset(zs, select = var_seleced_sd$Variabile)
# 
# # Elimina tutte le variabili RS dal dataset
# # zs <- subset(zs, select = -c(ndwi, ndvi, gndvi, green, texture))
# 
# # # Converti il dataframe in un dataframe standard
# # zs_standard <- as.data.frame(zs_selected)
# 
# # zs_standard$gndvi <- NULL
# 
# # Carica il pacchetto usdm
# library(usdm)
# 
# # Inizializza un vettore per memorizzare i nomi delle variabili da eliminare
# variables_to_remove <- c()
# 
# # Numero di cicli desiderati
# num_cycles <- 20
# 
# for (cycle in 1:num_cycles) {
#   # Seleziona le variabili da includere
#   selected <- zs_standard[, !names(zs_standard) %in% variables_to_remove]
#   
#   # Calcola i VIF per ogni variabile
#   vif_values <- vifstep(selected, th = 10)
#   
#   # Creazione del dataframe per i risultati dei VIF
#   vif_dataframe <- data.frame(Variables = names(selected), VIF = vif_values$VIF)
#   
#   # Rinomina le colonne nel dataframe vif_dataframe
#   colnames(vif_dataframe) <- c("Variables", "VIF")
#   
#   # Ordina il dataframe dei VIF in ordine crescente
#   vif_dataframe <- vif_dataframe[order(vif_dataframe$VIF, decreasing = FALSE), ]
#   
# # Rimuovi le variabili con VIF infinito o mancanti
#   # vif_dataframe <- vif_dataframe[is.finite(vif_dataframe$VIF), ]
#   
#   # Crea un grafico a barre dei VIF con limite sull'asse Y
#   barplot(vif_dataframe$VIF, names.arg = vif_dataframe$Variables, las = 2, col = "skyblue", main = "VIF per variabile", xlab = "Variabile", ylab = "VIF", ylim = c(0, 10))
#   
#   # Selezione manuale della variabile da eliminare
#   cat("Seleziona il nome della variabile da eliminare:", "\n")
#   var_to_remove <- readline(prompt = "> ")
#   
#   # Aggiungi la variabile alla lista delle variabili da eliminare
#   variables_to_remove <- c(variables_to_remove, var_to_remove)
# }

vif_values <- vifcor(zs_standard, th = 0.7, method = "pearson", keep = c('roads', "rivers"))

vif_variables <- vif_values@results$Variables

# Seleziona solo le variabili desiderate
zs_vif_0 <- zs_standard[, vif_variables]

cor_matrix <- cor(zs_vif_0, use = "complete", method = "pearson")

# Visualizza la correlazione
corrplot(cor_matrix, method = "color", type = "upper", 
         tl.col = "black", tl.srt = 45, tl.cex = 0.7, 
         addCoef.col = "black", number.cex = 0.45)

vif_values_2 <- vifstep(zs_vif_0, th = 10, keep = c('roads', 'rivers')) # è stata esclusa solo bkd

vif_variables_2 <- vif_values_2@results$Variables

# Seleziona solo le variabili desiderate
zs_vif_0 <- zs_standard[, vif_variables_2]

cor_matrix <- cor(zs_vif_0, use = "complete", method = "pearson")

# Visualizza la correlazione
corrplot(cor_matrix, method = "color", type = "upper", 
         tl.col = "black", tl.srt = 45, tl.cex = 0.7, 
         addCoef.col = "black", number.cex = 0.45)

vif_variables_2




vif_variables_2@results$Variables









