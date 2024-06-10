################################################################################

#                            Ensemble Models Evaluation

################################################################################

################################################################################
# Load all packages if necessary
library(gridExtra);library(cowplot);library(ggpubr);library(dplyr); library(biomod2)

# ## Load, if necessary, the previous model
(bm_out_file <- load("./Phytophthora/Phytophthora.20240604_1618_19.ensemble.models.out"))
#
myBiomodEM <- get(bm_out_file)
rm(list = c(bm_out_file, 'bm_out_file'))

############################ Get evaluations ###################################

# Get evaluation scores & variables importance
evaluations_df <- get_evaluations(myBiomodEM)

# Filtrare il dataframe escludendo le righe con algo = "SRE" o "MAXNET"
# evaluations_df <- evaluations_df[!(evaluations_df$algo %in% c("SRE", "MAXNET")), ]

################################################

# # Definisci la lista delle metriche
# metrics <- c("calibration", "validation")
# 
# # Crea una lista per memorizzare i grafici
# plot_list_calibration <- list()
# plot_list_validation <- list()
# 
# # Itera attraverso le metriche
# for (metric in metrics) {
#   # Calcola la media e la deviazione standard per ogni combinazione di PA, algoritmo e metrica
#   mean_sd_df <- evaluations_df %>%
#     group_by(PA, algo, metric.eval) %>%
#     summarise(mean_value = mean(get(metric)),
#               sd_value = ifelse(n() >= 2, sd(get(metric)), NA))
#   
#   # Filtra i dati per la metrica TSS
#   mean_sd_df_metric <- mean_sd_df %>%
#     filter(metric.eval == "TSS")
#   
#   # Filtra i dati per la metrica ROC
#   mean_sd_df_roc <- mean_sd_df %>%
#     filter(metric.eval == "ROC")
#   
#   # Conversione della variabile PA in un fattore
#   mean_sd_df_metric$PA <- factor(mean_sd_df_metric$PA)
#   mean_sd_df_roc$PA <- factor(mean_sd_df_roc$PA)
#   
#   # Unisci i dataframe mean_sd_df_metric e mean_sd_df_roc
#   merged_sd_df <- merge(mean_sd_df_metric, mean_sd_df_roc, by = c("PA", "algo"))
#   
#   # Crea i grafici per ogni PA
#   for (pa in unique(merged_sd_df$PA)) {
#     # Filtra il dataframe per PA e gli algoritmi desiderati
#     pa_df <- merged_sd_df[merged_sd_df$PA == pa, ]
#     
#     # Filtra i dati per gli algoritmi specifici per ogni PA
#     if (pa == "PA1") {
#       pa_df <- pa_df[pa_df$algo %in% c("CTA", "FDA", "GBM", "RF", "XGBOOST"), ]
#     } else if (pa == "PA2") {
#       pa_df <- pa_df[pa_df$algo %in% c("MARS", "SRE"), ]
#     } else if (pa == "PA3") {
#       pa_df <- pa_df[pa_df$algo %in% c("GLM", "GAM", "MAXENT", "MAXNET"), ]
#     }
#     
#     # Crea il grafico a dispersione con deviazione standard come linea
#     pa_plot <- ggplot(pa_df, aes(x = mean_value.y, y = mean_value.x, color = algo, group = interaction(PA, algo))) +
#       geom_point(size = 1.5) +
#       geom_line(linewidth = 1) +
#       geom_crossbar(aes(ymin = mean_value.x - sd_value.x, ymax = mean_value.x + sd_value.x), width = 0, fatten = 2) +
#       geom_crossbar(aes(xmin = mean_value.y - sd_value.y, xmax = mean_value.y + sd_value.y), width = 0, fatten = 2) +
#       labs(x = "ROC", y = "TSS") +
#       theme_minimal() +
#       facet_wrap(~PA) +
#       scale_color_brewer(palette = "Set1") +
#       guides(color = guide_legend(title = "Model")) +
#       xlim(0, 1) +  # Imposta i limiti dell'asse x da 0 a 1
#       ylim(0, 1) +  # Imposta i limiti dell'asse y da 0 a 1
#       ggtitle(paste("PA:", pa, "-", "Metric:", metric))  # Aggiunge titolo
#     
#     # Aggiungi il grafico alla lista corrispondente alla metrica
#     if (metric == "calibration") {
#       plot_list_calibration[[paste(pa, metric, sep = "_")]] <- pa_plot
#     } else {
#       plot_list_validation[[paste(pa, metric, sep = "_")]] <- pa_plot
#     }
#   }
# }
# 
# # Crea il layout dei grafici per il calibration
# arranged_plots_calibration <- grid.arrange(grobs = plot_list_calibration, ncol = 2)
# 
# # Crea il layout dei grafici per il validation
# arranged_plots_validation <- grid.arrange(grobs = plot_list_validation, ncol = 2)
# 
# # Organizza i due layout uno sopra l'altro
# final_layout <- grid.arrange(arranged_plots_calibration, arranged_plots_validation, nrow = 2)
# 
# # Visualizza il layout dei grafici
# print(arranged_plots_calibration)
# print(arranged_plots_validation)
# 
# print(final_layout)

###########

library(gridExtra); library(ggplot2)

# Rimuovere le righe con valori non definiti
evaluations_df_plot <- evaluations_df[complete.cases(evaluations_df[, c("calibration", "evaluation")]), ]

evaluations_df_plot$validation <- NULL

# Filtrare solo le righe con le metriche desiderate
# metrics_to_plot <- c("FAR", "SR", "BOYCE", "ROC", "TSS", "KAPPA", "ACCURACY", "BIAS")

metrics_to_plot <- c("ROC", "TSS", "ACCURACY")

evaluations_df_filtered <- evaluations_df_plot[evaluations_df_plot$metric.eval %in% metrics_to_plot, ]

# Creare una lista di grafici per le metriche desiderate
plots <- lapply(metrics_to_plot, function(metric) {
  # Filtrare il dataframe per il valore specifico di metric.eval
  df <- evaluations_df_filtered[evaluations_df_filtered$metric.eval == metric, ]
  
  # Creare il boxplot per la calibrazione
  plot_calibration <- ggplot(df, aes(x = algo, y = calibration)) +
    geom_boxplot() +
    labs(x = "Algoritmo", y = "Calibration", title = paste("Calibration - ", metric)) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    ylim(0, 1)
  
  # Creare il boxplot per l'evaluation
  plot_evaluation <- ggplot(df, aes(x = algo, y = evaluation)) +
    geom_boxplot() +
    labs(x = "Algoritmo", y = "Evaluation", title = paste("Evaluation - ", metric)) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    ylim(0, 1)
  
  # Ritornare una lista contenente i tre boxplot
  return(list(plot_calibration, plot_evaluation))
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
plots_per_sheet <- ceiling(4)

# Dividere la lista di grafici in tre parti
plots_sheet1 <- plots_flat[1:plots_per_sheet]
# plots_sheet2 <- plots_flat[(plots_per_sheet + 1):(2 * plots_per_sheet)]
# plots_sheet3 <- plots_flat[((2 * plots_per_sheet) + 1):total_plots]

# Creare i fogli separati
sheet1 <- plot_grid(plotlist = plots_sheet1, ncol = 2)
# sheet2 <- plot_grid(plotlist = plots_sheet2, ncol = 3)
# sheet3 <- plot_grid(plotlist = plots_sheet3, ncol = 3)

# Visualizzare i fogli separati
print(sheet1)
# print(sheet2)
# print(sheet3)































##################################

# Represent evaluation scores & variables importance
bm_PlotEvalMean(bm.out = myBiomodModelOut, 
                group.by = c('algo'), 
                dataset = "calibration", 
                do.plot = TRUE,
                metric.eval = c("TSS", "ROC"),
                xlim = c(0, 1),  # Modifica i limiti dell'asse x
                ylim = c(0, 1),  # Modifica i limiti dell'asse y
                main = "Mean Evaluation Scores - calibration")  # Modifica il titolo del grafico

# Represent evaluation scores & variables importance
bm_PlotEvalMean(bm.out = myBiomodModelOut, 
                group.by = c('algo'), 
                dataset = "validation", 
                do.plot = TRUE,
                metric.eval = c("TSS", "ROC"),
                xlim = c(0, 1),  # Modifica i limiti dell'asse x
                ylim = c(0, 1),  # Modifica i limiti dell'asse y
                main = "Mean Evaluation Scores - validation")  # Modifica il titolo del grafico

# Represent evaluation scores & variables importance
bm_PlotEvalMean(bm.out = myBiomodModelOut, 
                group.by = c('algo'), 
                dataset = "evaluation", 
                do.plot = TRUE,
                metric.eval = c("TSS", "ROC"),
                xlim = c(0, 1),  # Modifica i limiti dell'asse x
                ylim = c(0, 1),  # Modifica i limiti dell'asse y
                main = "Mean Evaluation Scores - calibration")  # Modifica il titolo del grafico



bm_PlotEvalMean(bm.out = myBiomodModelOut, 
                group.by = c('algo'), 
                dataset = "validation", 
                do.plot = TRUE, 
                metric.eval = c("FAR", "SR"),
                xlim = c(0, 1),  # Modifica i limiti dell'asse x
                ylim = c(0, 1),  # Modifica i limiti dell'asse y
                main = "Mean Evaluation Scores - validation")  # Modifica il titolo del grafico

bm_PlotEvalBoxplot(bm.out = myBiomodModelOut, 
                   group.by = c('algo', 'PA'), 
                   dataset = "validation",
                   do.plot = TRUE)

bm_PlotEvalBoxplot(bm.out = myBiomodModelOut, group.by = c('algo', 'run'))

library(gridExtra)
library(ggplot2)

# Rimuovere le righe con valori non definiti
evaluations_df_plot <- evaluations_df[complete.cases(evaluations_df[, c("calibration", "validation", "evaluation")]), ]

# Filtrare solo le righe con le metriche desiderate
metrics_to_plot <- c("FAR", "SR", "BOYCE", "ROC", "TSS", "KAPPA", "ACCURACY", "BIAS")
evaluations_df_filtered <- evaluations_df_plot[evaluations_df_plot$metric.eval %in% metrics_to_plot, ]

# Creare una lista di grafici per le metriche desiderate
plots <- lapply(metrics_to_plot, function(metric) {
  # Filtrare il dataframe per il valore specifico di metric.eval
  df <- evaluations_df_filtered[evaluations_df_filtered$metric.eval == metric, ]
  
  # Creare il boxplot per la calibrazione
  plot_calibration <- ggplot(df, aes(x = algo, y = calibration)) +
    geom_boxplot() +
    labs(x = "Algoritmo", y = "Calibration", title = paste("Calibration - ", metric)) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  # Creare il boxplot per la validazione
  plot_validation <- ggplot(df, aes(x = algo, y = validation)) +
    geom_boxplot() +
    labs(x = "Algoritmo", y = "Validation", title = paste("Validation - ", metric)) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  # Creare il boxplot per l'evaluation
  plot_evaluation <- ggplot(df, aes(x = algo, y = evaluation)) +
    geom_boxplot() +
    labs(x = "Algoritmo", y = "Evaluation", title = paste("Evaluation - ", metric)) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  # Ritornare una lista contenente i tre boxplot
  return(list(plot_calibration, plot_validation, plot_evaluation))
})

library(cowplot)

# Estrarre i singoli grafici dalla lista di liste
plots_flat <- unlist(plots, recursive = FALSE)

# Organizzare i boxplot in una griglia con calibrazione sopra, validazione al centro e evaluation sotto
arranged <- plot_grid(
  arrangeGrob(plots_flat[[1]], plots_flat[[2]], plots_flat[[3]], nrow = 3),
  arrangeGrob(plots_flat[[4]], plots_flat[[5]], plots_flat[[6]], nrow = 3),
  arrangeGrob(plots_flat[[7]], plots_flat[[8]], NULL, nrow = 3),
  ncol = 3
)

arranged

library(cowplot)

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

