################################################################################

#                        Variables Selection

################################################################################

library(readr); library(usdm); library(corrplot)

# Imposta il seed per rendere le analisi ripetibili
set.seed(999)

environmental <- rast("./INPUT/RASTER/environmental_50m.tif")

# Estrai le coordinate delle celle e i valori di tutti i layer
environmental_df <- as.data.frame(environmental, xy = FALSE, na.rm=TRUE)

zs <- environmental_df

zs$occ <- NULL

# zs$sand <- NULL
# 
# zs$silt <- NULL
# 
# zs$clay <- NULL
# 
# zs$texture <- NULL
# 
# zs$red2021 <- NULL
# 
# zs$red2023 <- NULL
# 
# zs$green2021 <- NULL
# 
# zs$green2023 <- NULL
# 
# zs$blue2021 <- NULL
# 
# zs$blue2023 <- NULL
# 
# zs$nir2021 <- NULL
# 
# zs$nir2023 <- NULL

# Converti il dataframe in un dataframe standard
zs_standard <- as.data.frame(zs)

options(scipen = 999)

vif_values <- vifcor(zs_standard, th = 0.7, method = "pearson", keep = c('roads', "rivers"))

# > vif_values
# 32 variables from the 53 input variables have collinearity problem: 
#   
#   BIO10 dgndvi gndvi2021 BIO18 gndvi2023 BIO01 BIO16 green2021 green2023 BIO08 elevation BIO05 BIO17 BIO12 ndvi2023 red2023 red2021 ndwi2023 ndwi2021 BIO02 blue2021 BIO09 BIO13 ndvi2021 BIO11 pH dndwi BIO14 nitrogen nir2023 BIO19 BIO15 
# 
# After excluding the collinear variables, the linear correlation coefficients ranges between: 
#   min correlation ( wo ~ fla ):  0.0007479791 
# max correlation ( BIO07 ~ BIO03 ):  0.6894874 
# 
# ---------- VIFs of the remained variables -------- 
#   Variables       VIF
# 1      roads  1.151388
# 2     rivers  1.438829
# 3     aspect  1.136341
# 4        fla  1.268546
# 5      slope  2.373696
# 6        tpi  1.270690
# 7        twi  2.299444
# 8       clay  6.154437
# 9        soc  3.920482
# 10      sand  8.158195
# 11      silt  9.245691
# 12   texture  1.176331
# 13       bkd 12.858758
# 14     BIO03 10.456554
# 15     BIO04  3.921327
# 16     BIO06 11.539104
# 17     BIO07 17.075860
# 18   nir2021  1.158507
# 19  blue2023  1.454398
# 20     dndvi  1.424320
# 21        wo  2.055014


vif_variables <- vif_values@results$Variables

# Seleziona solo le variabili desiderate
zs_vif_0 <- zs_standard[, vif_variables]

cor_matrix <- cor(zs_vif_0, use = "complete", method = "pearson")

# Visualizza la correlazione
corrplot(cor_matrix, method = "color", type = "upper", 
         tl.col = "black", tl.srt = 45, tl.cex = 0.7, 
         addCoef.col = "black", number.cex = 0.45)
