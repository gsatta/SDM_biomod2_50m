################################################################################

#                    Ensemble Models variable importance

################################################################################

EM_var_imp <-get_variables_importance(myBiomodEM)

bm_PlotVarImpBoxplot(bm.out = myBiomodEM, group.by = c('expl.var', 'full.name', 'full.name'))
bm_PlotVarImpBoxplot(bm.out = myBiomodEM, group.by = c('expl.var', 'algo', 'merged.by.run'))
bm_PlotVarImpBoxplot(bm.out = myBiomodEM, group.by = c('algo', 'expl.var', 'merged.by.run'))
                     
                     
EM_var_imp

EM_var_imp


# Carica i pacchetti necessari
library(ggplot2)

# Filtra il dataframe per contenere solo i dati relativi all'algoritmo "EMmedian"
EM_var_imp_EMmedian <- subset(EM_var_imp, algo == "EMmedian")

# Crea un grafico a barre dell'importanza delle variabili per l'algoritmo "EMmedian"
grafico_var_imp_EMmedian <- ggplot(EM_var_imp_EMmedian, aes(x = reorder(expl.var, var.imp), y = var.imp, fill = algo)) +
  geom_bar(stat = "identity", color = "black") +
  labs(x = "Variable", y = "Importance", fill = "Algoritmo") +
  scale_fill_manual(values = c("EMmedian" = "gray")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title = element_text(size = 12, face = "bold"),
        legend.position = "none",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(colour = "black", fill = NA, size = 1))

# Mostra il grafico
print(grafico_var_imp_EMmedian)

# Carica il pacchetto RColorBrewer
library(RColorBrewer)

# Crea una palette con 8 colori
palette_8_colors <- brewer.pal(8, "Set1")

# Mostra i colori della palette
palette_8_colors


# Crea un box plot dell'importanza delle variabili per l'algoritmo "EMmedian" con colori diversi per ogni variabile esplicativa e senza outlier
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




