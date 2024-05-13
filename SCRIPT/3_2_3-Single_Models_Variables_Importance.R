################################################################################

#                      Single Models Variables Importance

################################################################################


var_imp <- get_variables_importance(myBiomodModelOut)


bm_PlotVarImpBoxplot(bm.out = myBiomodModelOut, group.by = c('expl.var', 'algo', 'algo'))
bm_PlotVarImpBoxplot(bm.out = myBiomodModelOut, group.by = c('expl.var', 'algo', 'run'))
bm_PlotVarImpBoxplot(bm.out = myBiomodModelOut, group.by = c('algo', 'expl.var', 'run'))



var_imp <- get_variables_importance(myBiomodModelOut)

library(dplyr)

# Raggruppa i dati per expl.var e calcola la media di var.imp
ranked_vars <- var_imp %>%
  group_by(expl.var) %>%
  summarize(mean_var_imp = mean(var.imp)) %>%
  # Ordina i gruppi in base alla media di var.imp in ordine decrescente
  arrange(desc(mean_var_imp)) %>%
  # Aggiungi una colonna con il rank
  mutate(rank = row_number())

# Stampare i risultati
print(ranked_vars)
