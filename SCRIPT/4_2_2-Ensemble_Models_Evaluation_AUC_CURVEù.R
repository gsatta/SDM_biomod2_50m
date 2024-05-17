################################################################################

#                            Ensemble Models Evaluation AUC CURVE

################################################################################
## formale response variable 
form.dat <- get_formal_data(myBiomodModelOut, 'resp.var')
length(form.dat)


## calibration lines
calib.lines <- get_calib_lines(myBiomodModelOut)
dim(calib.lines)
dimnames(calib.lines)


## models predictions
pred.val <- get_predictions(myBiomodModelOut) 
dim(pred.val)
dimnames(pred.val)


## get all combination of model, cv run, pa data
model.comb <- 
  expand.grid(
    mod = dimnames(pred.val)[[2]],
    pa = dimnames(pred.val$PA),
    stringsAsFactors = FALSE
  ) 

## compute all the roc cuurves
mod.roc <- lapply(
  1:nrow(model.comb),
  function(i){
    mod <- colnames(calib.lines)[i]
    pa <- gsub("^.*_RUN([0-9]+)$", "\\1", mod)
    mod <- gsub("^.*_RUN[0-9]+_(.*)$", "\\1", mod)
    
    eval.lines <- !calib.lines[, i]
    
    resp <- form.dat[eval.lines]
    pred <- pred.val[eval.lines, i]
    
    if (is.numeric(pred)) {
      pred <- pred / 1000
      pROC::roc(resp, pred)
    } else {
      warning("pred.val[eval.lines, ", i, "] is not numeric")
      NULL
    }
  }
)

## plot roc curves
par(mfrow = c(2,2)) 
lapply(mod.roc, plot)



