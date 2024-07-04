################################################################################

#                            Single Models Evaluation

################################################################################
# Load all packages if necessary
library(gridExtra);library(cowplot);library(ggpubr);library(dplyr); library(biomod2)

# # #Load, if necessary, the previous model
(bm_out_file <- load("./Phytophthora/Phytophthora.20240704_1446_08.models.out"))
# 
myBiomodModelOut <- get(bm_out_file)
rm(list = c(bm_out_file, 'bm_out_file'))

############################ Get evaluations ###################################

# Get evaluation scores & variables importance
evaluations_df <- get_evaluations(myBiomodModelOut)

# Filtrare il dataframe escludendo le righe con algo = "SRE" o "MAXNET"
# evaluations_df <- evaluations_df[!(evaluations_df$algo %in% c("SRE", "MAXNET")), ]

################################################

library(gridExtra); library(ggplot2)

# Rimuovere le righe con valori non definiti
evaluations_df_plot <- evaluations_df[complete.cases(evaluations_df[, c("calibration", "validation", "evaluation")]), ]

# Filtrare solo le righe con le metriche desiderate
# metrics_to_plot <- c("FAR", "SR", "BOYCE", "ROC", "TSS", "KAPPA", "ACCURACY", "BIAS")

metrics_to_plot <- c("POD", "FAR", "POFD", "SR", "ACCURACY", "BIAS", "ROC", "TSS",
                     "KAPPA", "OR", "ORSS", "CSI", "ETS", "BOYCE", "MPA")

# metrics_to_plot <- c("ROC", "KAPPA", "TSS")

evaluations_df_filtered <- evaluations_df_plot[evaluations_df_plot$metric.eval %in% metrics_to_plot, ]

# Creare una lista di grafici per le metriche desiderate
plots <- lapply(metrics_to_plot, function(metric) {
  # Filtrare il dataframe per il valore specifico di metric.eval
  df <- evaluations_df_filtered[evaluations_df_filtered$metric.eval == metric, ]
  
  # Creare il boxplot per la calibrazione
  plot_calibration <- ggplot(df, aes(x = algo, y = calibration)) +
    geom_boxplot() +
    labs(x = "Algorithm", y = "Calibration", title = paste("Calibration - ", metric)) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    ylim(0, 1)
  
  # Creare il boxplot per la validazione
  plot_validation <- ggplot(df, aes(x = algo, y = validation)) +
    geom_boxplot() +
    labs(x = "Algoritmo", y = "Validation", title = paste("Validation - ", metric)) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    ylim(0, 1)
  
  # Creare il boxplot per l'evaluation
  plot_evaluation <- ggplot(df, aes(x = algo, y = evaluation)) +
    geom_boxplot() +
    labs(x = "Algoritmo", y = "Evaluation", title = paste("Evaluation - ", metric)) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    ylim(0, 1)
  
  # Ritornare una lista contenente i tre boxplot
  return(list(plot_calibration, plot_validation, plot_evaluation))
})

library(cowplot)

# Estrarre i singoli grafici dalla lista di liste
plots_flat <- unlist(plots, recursive = FALSE)

# # Organizzare i boxplot in una griglia con calibrazione sopra, validazione al centro e evaluation sotto
# arranged <- plot_grid(
#   arrangeGrob(plots_flat[[1]], plots_flat[[2]], plots_flat[[3]], nrow = 3),
#   arrangeGrob(plots_flat[[4]], plots_flat[[5]], plots_flat[[6]], nrow = 3),
#   arrangeGrob(plots_flat[[7]], plots_flat[[8]], NULL, nrow = 3),
#   ncol = 3
# )
# 
# arranged

library(cowplot)

# Numero totale di grafici
total_plots <- length(plots_flat)

# Numero di grafici per foglio
plots_per_sheet <- ceiling(9)

# Dividere la lista di grafici in tre parti
plots_sheet1 <- plots_flat[1:plots_per_sheet]
plots_sheet2 <- plots_flat[(plots_per_sheet + 1):(2 * plots_per_sheet)]
plots_sheet3 <- plots_flat[((2 * plots_per_sheet) + 1):total_plots]

# Creare i fogli separati
sheet1 <- plot_grid(plotlist = plots_sheet1, ncol = 3)
sheet2 <- plot_grid(plotlist = plots_sheet2, ncol = 3)
sheet3 <- plot_grid(plotlist = plots_sheet3, ncol = 3)

# Visualizzare i fogli separati
print(sheet1)
print(sheet2)
print(sheet3)

