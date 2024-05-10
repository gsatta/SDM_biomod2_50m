################################################################################

#                      Single Models Variables Importance

################################################################################



bm_PlotVarImpBoxplot(bm.out = myBiomodEnsembleOut, group.by = c('expl.var', 'algo', 'algo'))
bm_PlotVarImpBoxplot(bm.out = myBiomodEnsembleOut, group.by = c('expl.var', 'algo', 'run'))
bm_PlotVarImpBoxplot(bm.out = myBiomodEnsembleOut, group.by = c('algo', 'expl.var', 'run'))

