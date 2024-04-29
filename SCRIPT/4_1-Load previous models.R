
library(ggplot2)
# Single models

(bm_out_file <- load("./Phytophthora/Phytophthora.20240427_1256_45.models.out"))

my_bm_model <- get(bm_out_file)
rm(list = c(bm_out_file, 'bm_out_file'))

# Represent evaluation scores & variables importance
bm_PlotEvalMean(bm.out = my_bm_model, 
                group.by = c('algo'), 
                dataset = "validation", 
                do.plot = TRUE, 
                xlim = c(0, 1),  # Modifica i limiti dell'asse x
                ylim = c(0, 1),  # Modifica i limiti dell'asse y
                main = "Mean Evaluation Scores")  # Modifica il titolo del grafico

(bm_out_file <- load("./Phytophthora/Phytophthora.20240415_1829_44.ensemble.models.out"))

my_bm_model <- get(bm_out_file)
rm(list = c(bm_out_file, 'bm_out_file'))

# Represent response curves
bm_PlotResponseCurves(bm.out = my_bm_model, 
                      models.chosen = get_built_models(my_bm_model)[1],
                      fixed.var = 'median') + facet_wrap(scales = "free_y")
bm_PlotResponseCurves(bm.out = my_bm_model, 
                      models.chosen = get_built_models(my_bm_model)[c(1:3, 12:14)],
                      fixed.var = 'min')
bm_PlotResponseCurves(bm.out = my_bm_model, 
                      models.chosen = get_built_models(my_bm_model)[3],
                      fixed.var = 'median',
                      do.bivariate = TRUE)

get_variables_importance(myBiomodEM)




