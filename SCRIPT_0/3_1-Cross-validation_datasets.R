# Cross-Validation datasets

# k-fold selection
cv_k <- bm_CrossValidation(bm.format = myBiomodData,
                           strategy = "kfold",
                           nb.rep = 2,
                           k = 3)

head(cv_k)

cv_s <- bm_CrossValidation(bm.format = myBiomodData,
                           strategy = "strat",
                           k = 2,
                           balance = "presences",
                           strat = "x")

head(cv_s, 2, table)

# random selection
cv.r <- bm_CrossValidation(bm.format = myBiomodData,
                           strategy = "random",
                           nb.rep = 3,
                           k = 0.8)

head(cv.r, 2, table)
