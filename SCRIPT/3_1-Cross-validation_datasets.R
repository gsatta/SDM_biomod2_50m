# Cross-Validation datasets

# k-fold selection
cv_k <- bm_CrossValidation(bm.format = myBiomodData,
                           strategy = "kfold",
                           nb.rep = 2,
                           k = 5)

head(cv_k, 2, table)

