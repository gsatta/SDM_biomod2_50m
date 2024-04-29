# Run modellization
path_to_maxent.jar <- file.path(getwd(), "maxent.jar")

maxent.background.dat.dir <- "./INPUT/RASTER/ASCII"

list.files(maxent.background.dat.dir, pattern = "\\.asc$")

myBiomodOption <- BIOMOD_ModelingOptions(
  MAXENT = list( path_to_maxent.jar =  path_to_maxent.jar,
                 background_data_dir = maxent.background.dat.dir,
                 maximumiterations = 200,
                 visible = FALSE,
                 linear = TRUE,
                 quadratic = TRUE,
                 product = TRUE,
                 threshold = TRUE,
                 hinge = TRUE,
                 lq2lqptthreshold = 80,
                 l2lqthreshold = 10,
                 hingethreshold = 15,
                 beta_threshold = -1,
                 beta_categorical = -1,
                 beta_lqp = -1,
                 beta_hinge = -1,
                 defaultprevalence = 0.5),
  # GLM = list(type = 'polynomial', interaction.level = 1),
  # GBM = list(n.trees = 1500),
  GAM = list(interaction.level = 1, type = 's_smoother', algo = 'GAM_mgcv'),
  # CTA = NULL,
  # # ANN = list(NbCV = 20, size = NULL, decay = NULL),
  # SRE = list(quant = 0.025),
  # FDA = NULL,
  # MARS = list(type = 'polynomial', interaction.level = 1),
  RF = list(do.classif = FALSE, ntree = 1000, importance = TRUE, nodesize = 10, maxnodes = 5)
  # XGBOOST = list(max.depth = 5, eta = 0.01, nrounds = 1000, objective = "binary:logistic", nthread = 1)
  )

# models = c('CTA', 'RF', 'GBM', 'FDA', 'MARS', 'GLM', 'GAM', 'MAXENT')

# single models
myBiomodModelOut <- BIOMOD_Modeling(
  bm.format = myBiomodData,
  bm.options = myBiomodOption,
  modeling.id = as.character(format(Sys.time(), "%Y%m%d_%H%M_%S")),
  models = c("ANN", "CTA", "FDA", "GAM", "GBM", "GLM", "MARS", "MAXENT", "MAXNET", "RF",
              "SRE", "XGBOOST"),
  CV.strategy = 'random',
  CV.nb.rep = 10,
  CV.perc = 0.8,
  var.import = 3,
  metric.eval = c("TSS", "ROC"),
  seed.val = 123,
  do.progress = TRUE
)




# # single models
# myBiomodModelOut_GAM_GLM_MAXENT <- BIOMOD_Modeling(
#   bm.format = myBiomodData_GAM_GLM_MAXENT,
#   bm.options = myBiomodOption,
#   modeling.id = paste("GAM_GLM_MAXENT_", as.character(format(Sys.time(), "%Y%m%d_%H%M_%S")), sep = "_"),
#   models = c('GAM', 'GLM', 'MAXENT'),
#   CV.strategy = 'random',
#   CV.nb.rep = 10,
#   CV.perc = 0.8,
#   var.import = 3,
#   metric.eval = c("TSS", "ROC"),
#   # metric.eval = c("ROC"),
#   # nb.cpu = 4,
#   seed.val = 123,
#   do.progress = TRUE
# )
# 
# # single models
# myBiomodModelOut_MARS <- BIOMOD_Modeling(
#   bm.format = myBiomodData_MARS,
#   bm.options = myBiomodOption,
#   modeling.id = paste("MARS_", as.character(format(Sys.time(), "%Y%m%d_%H%M_%S")), sep = "_"),
#   models = c('MARS'),
#   CV.strategy = 'random',
#   CV.nb.rep = 10,
#   CV.perc = 0.8,
#   var.import = 3,
#   metric.eval = c("TSS", "ROC"),
#   # metric.eval = c("ROC"),
#   # nb.cpu = 4,
#   seed.val = 123,
#   do.progress = TRUE
# )
# 
# # single models
# myBiomodModelOut_CTA_RF_GBM_FDA <- BIOMOD_Modeling(
#   bm.format = myBiomodData_CTA_RF_GN_FDA,
#   bm.options = myBiomodOption,
#   modeling.id = paste("CTA_RF_GBM_FDA_", as.character(format(Sys.time(), "%Y%m%d_%H%M_%S")), sep = "_"),
#   models = c('CTA', 'RF', 'GBM', 'FDA'),
#   CV.strategy = 'random',
#   CV.nb.rep = 10,
#   CV.perc = 0.8,
#   var.import = 3,
#   metric.eval = c("TSS", "ROC"),
#   # metric.eval = c("ROC"),
#   # nb.cpu = 4,
#   seed.val = 123,
#   do.progress = TRUE
# )
# 
# 
# myBiomodModelOut <- c(myBiomodModelOut_MARS, myBiomodModelOut_GAM_GLM_MAXENT, myBiomodModelOut_CTA_RF_GBM_FDA)


(bm_out_file <- load("./Phytophthora/Phytophthora.20240427_1417_51.models.out"))

myBiomodModelOut <- get(bm_out_file)
rm(list = c(bm_out_file, 'bm_out_file'))

# Get evaluation scores & variables importance
models_score <- get_evaluations(myBiomodModelOut)
var_imp <- get_variables_importance(myBiomodModelOut)

evaluations_df <-  as.data.frame(models_score)

# Filtrare il dataframe escludendo le righe con algo = "SRE" o "MAXNET"
evaluations_df <- evaluations_df[!(evaluations_df$algo %in% c("SRE", "MAXNET")), ]

#################################################
library(gridExtra)
library(cowplot)
library(ggpubr)
library(dplyr)

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
      pa_df <- pa_df[pa_df$algo %in% c("CTA", "FDA", "GBM", "RF", "ANN", "XGBOOST"), ]
    } else if (pa == "PA2") {
      pa_df <- pa_df[pa_df$algo %in% c("MARS", "SRE"), ]
    } else if (pa == "PA3") {
      pa_df <- pa_df[pa_df$algo %in% c("GLM", "GAM", "MAXENT", "MAXNET"), ]
    }
    
    # Crea il grafico a dispersione con deviazione standard come linea
    pa_plot <- ggplot(pa_df, aes(x = mean_value.y, y = mean_value.x, color = algo, group = interaction(PA, algo))) +
      geom_point(size = 1.5) +
      geom_line(size = 1) +
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

# Visualizza il layout dei grafici
print(arranged_plots_calibration)
print(arranged_plots_validation)


##################################

## S4 method for signature 'BIOMOD.ensemble.models.out'

eval_PA1 <- get_evaluations(
  myBiomodModelOut,
  algo = c('CTA', 'RF', 'GBM', 'FDA'),
  metric.eval = c('TSS', 'ROC')
)

a_df <- as.data.frame(a)

get_evaluations(
  myBiomodModelOut,
  PA = 'PA1',
  #full.name = ,
  merged.by.algo = c('CTA', 'RF', 'GBM', 'FDA'),
  merged.by.PA = 'PA1',
  algo = c('CTA', 'RF', 'GBM', 'FDA'),
  metric.eval = c('TSS', 'ROC')
)

get_evaluations(
  obj,
  full.name = NULL,
  merged.by.algo = NULL,
  merged.by.run = NULL,
  merged.by.PA = NULL,
  filtered.by = NULL,
  algo = NULL,
  metric.eval = c('TSS', 'ROC')
)



# Creazione del dataframe con le metriche del modello
model_metrics <- as.data.frame(myBiomodModelOut@models.evaluation@val)

# Visualizzazione del dataframe
print(model_metrics)

dim(models_score)
dimnames(models_score)

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
bm_PlotVarImpBoxplot(bm.out = myBiomodModelOut, group.by = c('expl.var', 'algo', 'algo'))
bm_PlotVarImpBoxplot(bm.out = myBiomodModelOut, group.by = c('expl.var', 'algo', 'run'))
bm_PlotVarImpBoxplot(bm.out = myBiomodModelOut, group.by = c('algo', 'expl.var', 'run'))

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

EN_models_score <- get_evaluations(myBiomodModelOut)

# laod the packages
library(precrec)
library(ggplot2) # for plotting the curves

# Estrai le probabilità predette per ogni modello
# Ottieni le probabilità predette per ogni modello
predicted_probs <- get_predictions(myBiomodModelOut)


# Estrai i nomi dei modelli e i rispettivi valori AUC
model_names <- models_score$full.name
roc_values <- models_score$validation

# Calcola l'AUC per la curva ROC
auc_value <- auc(roc_values)

# Crea un grafico a barre per visualizzare l'AUC per ciascun modello
barplot(auc_values, names.arg = model_names, main = "AUC per modello", xlab = "Modello", ylab = "AUC", col = "skyblue", las = 2)




