################################################################################

#                            Single Models Evaluation

################################################################################
# Load all packages if necessary
library(gridExtra);library(cowplot);library(ggpubr);library(dplyr); library(biomod2)

# Load, if necessary, the previous model
(bm_out_file <- load("./Phytophthora/Phytophthora.20240501_1439_25.models.out"))

myBiomodModelOut <- get(bm_out_file)
rm(list = c(bm_out_file, 'bm_out_file'))

############################ Get evaluations ###################################

# Get evaluation scores & variables importance
evaluations_df <- get_evaluations(myBiomodModelOut)
var_imp <- get_variables_importance(myBiomodModelOut)


# Evaluate presence only models
evaluations_df <- BIOMOD_PresenceOnly(bm.mod = myBiomodModelOut, 
                                      bg.env = myExpl,
                                      perc = 0.9)

# Filtrare il dataframe escludendo le righe con algo = "SRE" o "MAXNET"
# evaluations_df <- evaluations_df[!(evaluations_df$algo %in% c("SRE", "MAXNET")), ]

################################################

# Definisci la lista delle metriche
metrics <- c("calibration", "validation")

# Crea una lista per memorizzare i grafici
plot_list_calibration <- list()
plot_list_validation <- list()

# Itera attraverso le metriche
for (metric in metrics) {
  # Calcola la media e la deviazione standard per ogni combinazione di PA, algoritmo e metrica
  mean_sd_df <- evaluations_df %>%
    group_by(PA, algo, metric.eval) %>%
    summarise(mean_value = mean(get(metric)),
              sd_value = ifelse(n() >= 2, sd(get(metric)), NA))
  
    # Filtra i dati per la metrica TSS
  mean_sd_df_metric <- mean_sd_df %>%
    filter(metric.eval == "TSS")
  
  # Filtra i dati per la metrica ROC
  mean_sd_df_roc <- mean_sd_df %>%
    filter(metric.eval == "ROC")
  
  # Conversione della variabile PA in un fattore
  mean_sd_df_metric$PA <- factor(mean_sd_df_metric$PA)
  mean_sd_df_roc$PA <- factor(mean_sd_df_roc$PA)
  
  # Unisci i dataframe mean_sd_df_metric e mean_sd_df_roc
  merged_sd_df <- merge(mean_sd_df_metric, mean_sd_df_roc, by = c("PA", "algo"))
  
  # Crea i grafici per ogni PA
  for (pa in unique(merged_sd_df$PA)) {
    # Filtra il dataframe per PA e gli algoritmi desiderati
    pa_df <- merged_sd_df[merged_sd_df$PA == pa, ]
    
    # Filtra i dati per gli algoritmi specifici per ogni PA
    if (pa == "PA1") {
      pa_df <- pa_df[pa_df$algo %in% c("CTA", "FDA", "GBM", "RF", "XGBOOST"), ]
    } else if (pa == "PA2") {
      pa_df <- pa_df[pa_df$algo %in% c("MARS", "SRE"), ]
    } else if (pa == "PA3") {
      pa_df <- pa_df[pa_df$algo %in% c("GLM", "GAM", "MAXENT", "MAXNET"), ]
    }
    
    # Crea il grafico a dispersione con deviazione standard come linea
    pa_plot <- ggplot(pa_df, aes(x = mean_value.y, y = mean_value.x, color = algo, group = interaction(PA, algo))) +
      geom_point(size = 1.5) +
      geom_line(linewidth = 1) +
      geom_crossbar(aes(ymin = mean_value.x - sd_value.x, ymax = mean_value.x + sd_value.x), width = 0, fatten = 2) +
      geom_crossbar(aes(xmin = mean_value.y - sd_value.y, xmax = mean_value.y + sd_value.y), width = 0, fatten = 2) +
      labs(x = "ROC", y = "TSS") +
      theme_minimal() +
      facet_wrap(~PA) +
      scale_color_brewer(palette = "Set1") +
      guides(color = guide_legend(title = "Model")) +
      xlim(0, 1) +  # Imposta i limiti dell'asse x da 0 a 1
      ylim(0, 1) +  # Imposta i limiti dell'asse y da 0 a 1
      ggtitle(paste("PA:", pa, "-", "Metric:", metric))  # Aggiunge titolo
    
    # Aggiungi il grafico alla lista corrispondente alla metrica
    if (metric == "calibration") {
      plot_list_calibration[[paste(pa, metric, sep = "_")]] <- pa_plot
    } else {
      plot_list_validation[[paste(pa, metric, sep = "_")]] <- pa_plot
    }
  }
}

# Crea il layout dei grafici per il calibration
arranged_plots_calibration <- grid.arrange(grobs = plot_list_calibration, ncol = 2)

# Crea il layout dei grafici per il validation
arranged_plots_validation <- grid.arrange(grobs = plot_list_validation, ncol = 2)

# Organizza i due layout uno sopra l'altro
final_layout <- grid.arrange(arranged_plots_calibration, arranged_plots_validation, nrow = 2)

# Visualizza il layout dei grafici
print(arranged_plots_calibration)
print(arranged_plots_validation)

print(final_layout)


##################################

# Represent evaluation scores & variables importance
bm_PlotEvalMean(bm.out = myBiomodModelOut, 
                group.by = c('algo'), 
                dataset = "validation", 
                do.plot = TRUE, 
                xlim = c(0, 1),  # Modifica i limiti dell'asse x
                ylim = c(0, 1),  # Modifica i limiti dell'asse y
                main = "Mean Evaluation Scores")  # Modifica il titolo del grafico

bm_PlotEvalMean(bm.out = myBiomodModelOut, 
                group.by = c('PA'), 
                dataset = "validation", 
                do.plot = TRUE, 
                xlim = c(0, 1),  # Modifica i limiti dell'asse x
                ylim = c(0, 1),  # Modifica i limiti dell'asse y
                main = "Mean Evaluation Scores")  # Modifica il titolo del grafico

bm_PlotEvalBoxplot(bm.out = myBiomodModelOut, 
                   group.by = c('algo', 'PA'), 
                   dataset = "validation",
                   do.plot = TRUE)

bm_PlotEvalBoxplot(bm.out = myBiomodModelOut, group.by = c('algo', 'run'))




