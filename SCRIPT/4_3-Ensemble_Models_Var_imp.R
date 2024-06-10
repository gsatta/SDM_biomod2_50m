################################################################################

#                    Ensemble Models variable importance

################################################################################
# Carica i pacchetti necessari
library(ggplot2)

EM_var_imp <-get_variables_importance(myBiomodEM)

# bm_PlotVarImpBoxplot(bm.out = myBiomodEM, group.by = c('expl.var', 'full.name', 'full.name'))
# bm_PlotVarImpBoxplot(bm.out = myBiomodEM, group.by = c('expl.var', 'algo', 'merged.by.run'))
# bm_PlotVarImpBoxplot(bm.out = myBiomodEM, group.by = c('algo', 'expl.var', 'merged.by.run'))

# Filtra il dataframe per contenere solo i dati relativi all'algoritmo "EMmedian"
EM_var_imp_EMmedian <- subset(EM_var_imp, algo == "EMmedian")

# Carica il pacchetto RColorBrewer
library(RColorBrewer)

# Crea una palette con 8 colori
palette_8_colors <- brewer.pal(8, "Set1")

# Mostra i colori della palette
palette_8_colors

# Crea un box plot 
grafico_boxplot_colored_no_outliers <- ggplot(EM_var_imp_EMmedian, aes(x = expl.var, y = var.imp, fill = expl.var)) +
  geom_boxplot(color = "black", alpha = 0.5, outlier.shape = NA) + # Nasconde gli outlier
  labs(x = "Variables", y = "Importance", fill = "Variables") +
  scale_fill_manual(values = palette_8_colors) + 
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title = element_text(size = 12, face = "bold"),
        legend.position = "none") + # Rimuove la legenda
  guides(fill = FALSE) # Rimuove la guida per il riempimento

# Mostra il box plot colorato senza gli outlier
print(grafico_boxplot_colored_no_outliers)

# Crea la cartella se non esiste
if (!dir.exists("./GRAPHS/")) {
  dir.create("./GRAPHS/")
}

# Salva il grafico in un file nella cartella ./GRAPHS/
ggsave("./GRAPHS/EM_imp_var_boxplot.jpg", plot = grafico_boxplot_colored_no_outliers, width = 10, height = 7, dpi = 300)


# Crea il grafico a barre
bar_imp <- ggplot(df_filtered, aes(x = expl.name, y = pred.val)) +
  geom_bar(stat = "summary", fun = "mean", fill = "skyblue") +
  theme_minimal() +
  labs(x = "Variables",
       y = "Importance")

plot(bar_imp)

# Salva il grafico in un file nella cartella ./GRAPHS/
ggsave("./GRAPHS/EM_bar_imp_plot.jpg", plot = bar_imp, width = 10, height = 7, dpi = 300)

