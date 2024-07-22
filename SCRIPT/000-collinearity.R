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

zs$sand <- NULL

zs$silt <- NULL

zs$clay <- NULL

zs$texture <- NULL

zs$red2021 <- NULL

zs$red2023 <- NULL

zs$green2021 <- NULL

zs$green2023 <- NULL

zs$blue2021 <- NULL

zs$blue2023 <- NULL

zs$nir2021 <- NULL

zs$nir2023 <- NULL

# Converti il dataframe in un dataframe standard
zs_standard <- as.data.frame(zs)

options(scipen = 999)

vif_values <- vifcor(zs_standard, th = 0.7, method = "pearson", keep = c('roads', "rivers"))

# 24 variables from the 41 input variables have collinearity problem: 
#   
#   BIO10 BIO18 gndvi2021 gndvi2023 dgndvi BIO01 BIO16 BIO08 elevation BIO05 BIO17 BIO12 ndvi2023 ndwi2023 ndvi2021 BIO02 BIO09 BIO13 pH BIO11 dndwi nitrogen BIO14 BIO19 
# 
# After excluding the collinear variables, the linear correlation coefficients ranges between: 
#   min correlation ( ndwi2021 ~ fla ):  0.0002827655 
# max correlation ( BIO15 ~ BIO04 ):  -0.6903187 
# 
# ---------- VIFs of the remained variables -------- 
#   Variables       VIF
# 1      roads  1.165192
# 2     rivers  1.485713
# 3     aspect  1.086053
# 4        fla  1.260795
# 5      slope  2.365855
# 6        tpi  1.256630
# 7        twi  2.213905
# 8        soc  3.048810
# 9        bkd  1.622017
# 10     BIO03 10.602690
# 11     BIO04  6.020717
# 12     BIO06 10.813018
# 13     BIO07 20.436007
# 14     BIO15  6.701226
# 15  ndwi2021  1.362858
# 16     dndvi  1.385212
# 17        wo  1.866192

vif_variables <- vif_values@results$Variables

# Seleziona solo le variabili desiderate
zs_vif_0 <- zs_standard[, vif_variables]

cor_matrix <- cor(zs_vif_0, use = "complete", method = "pearson")

# Visualizza la correlazione
corrplot(cor_matrix, method = "color", type = "upper", 
         tl.col = "black", tl.srt = 45, tl.cex = 0.7, 
         addCoef.col = "black", number.cex = 0.45)

# > vif_values_2
# 2 variables from the 17 input variables have collinearity problem: 
#   
#   BIO07 BIO04 
# 
# After excluding the collinear variables, the linear correlation coefficients ranges between: 
#   min correlation ( BIO15 ~ fla ):  -0.0003875331 
# max correlation ( BIO03 ~ soc ):  -0.5866884 
# 
# ---------- VIFs of the remained variables -------- 
#   Variables      VIF
# 1      roads 1.143632
# 2     rivers 1.388255
# 3     aspect 1.073247
# 4        fla 1.276774
# 5      slope 2.224088
# 6        tpi 1.264534
# 7        twi 2.245807
# 8        soc 2.884837
# 9        bkd 1.422554
# 10     BIO03 2.221509
# 11     BIO06 1.958931
# 12     BIO15 1.421190
# 13  ndwi2021 1.307304
# 14     dndvi 1.257369
# 15        wo 1.618243


vif_variables_2 <- vif_values_2@results$Variables

# Seleziona solo le variabili desiderate
zs_vif_0 <- zs_standard[, vif_variables_2]

cor_matrix <- cor(zs_vif_0, use = "complete", method = "pearson")

# Visualizza la correlazione
corrplot(cor_matrix, method = "color", type = "upper", 
         tl.col = "black", tl.srt = 45, tl.cex = 0.7, 
         addCoef.col = "black", number.cex = 0.45)

vif_variables_2




vif_variables_2@results$Variables









