################################################################################

#                   Ensemble Models response curves

################################################################################
# Carica i pacchetti necessari
library(ggplot2)

# Represent response curves
resp_models <- bm_PlotResponseCurves(bm.out = myBiomodEM, 
                        models.chosen = get_built_models(myBiomodEM)[c(1)])
                                          
# Assumiamo che il tuo dataframe sia `resp_models$tab`
df <- resp_models$tab

# Filtra il dataframe per rimuovere le righe con expl.name "wo" e "fla"
df_filtered <- subset(df, !(expl.name %in% c("wo", "fla")))

# Definisci l'ordine personalizzato dei livelli di expl.name
customized_order <- c("green", "BIO03", "BIO06", "soc", "rivers", "roads")

# Converti expl.name in un fattore con ordine personalizzato
df_filtered$expl.name <- factor(df_filtered$expl.name, levels = customized_order)

# Definisci nuovi nomi per le variabili
new_names <- c(
  "green" = "Green",
  "BIO03" = "BIO03 = Isothermality (BIO2/BIO7) * (×100)",
  "BIO06" = "BIO06 = Min Temperature of Coldest Month (°C)",
  "soc" = "Soil Organic Carbon (g/kg)",
  "rivers" = "Distance from rivers (m)",
  "roads" = "Distance from roads (m)"
)

# Crea il grafico con curve più fini, spessore della linea più sottile, nomi delle variabili sotto i grafici in grassetto e quadrati attorno a ogni grafico
p_median <- ggplot(df_filtered, aes(x = expl.val, y = pred.val)) +
  geom_smooth(method = "loess", color = "blue", fill = "gray45", se = TRUE, alpha = 0.5, linewidth  = 0.5) +
  facet_wrap(~ expl.name, scales = "free_x", strip.position = "bottom", labeller = labeller(expl.name = new_names)) +
  labs(x = "",
       y = "Probability (%)") +
  theme_minimal() +
  theme(strip.background = element_blank(),
        strip.placement = "outside",
        strip.text = element_text(face = "bold"),
        panel.border = element_rect(color = "black", fill = NA))


# Mostra il grafico
print(p_median)

# Crea la cartella se non esiste
if (!dir.exists("./GRAPHS/")) {
  dir.create("./GRAPHS/")
}

# Salva il grafico in un file nella cartella ./GRAPHS/
ggsave("./GRAPHS/response_curve.jpg", plot = p_median, width = 10, height = 7, dpi = 300)