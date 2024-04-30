################################################################################

#                         Density Graphs Variables

################################################################################

# Load the necessary packages
library(terra); library(sf); library(ggplot2); library(gridExtra); library(grid)

# Extract the value of the predictor data where we have the Phytophthora
myExpl_0 <- rast("./INPUT/RASTER/enviromental_50m.tiff")

# Scale the pH values at the own usual  scale
myExpl_0$pH <- myExpl_0$pH/10

# Delete the phytophthora and texture layer
myExpl_0$phytophthora <- NULL
myExpl_0$texture <- NULL

# Load the occurrence data
DataSpecies_0 <- st_read("./INPUT/VECTOR/p-psa_adj.gpkg")

# Select only the rows where presence == 1
DataSpecies_0 <- DataSpecies_0[DataSpecies_0$presence == 1, ]

# Extract the pixels value of the environmental raster 
extracted <- terra::extract(myExpl_0, DataSpecies_0)

# Delete the ID column
extracted$ID <- NULL

############################ Create the graphs #################################

# Numero totale di variabili
num_variabili <- ncol(extracted)

# Crea una lista per memorizzare i grafici di tutti i fogli
plot_list <- list()

# Ciclo per creare i grafici di tutte le variabili
for (i in 1:num_variabili) {
  # Crea un nuovo grafico utilizzando ggplot2 e lo aggiunge alla lista
  plot_list[[i]] <- ggplot(data = extracted, aes(x = .data[[names(extracted)[i]]])) +
    geom_histogram(aes(y = after_stat(density)), bins = 30, fill = "lightgray", color = "black") +
    geom_density(color = "red") +
    labs(x = "Value", y = "Density", title = names(extracted)[i])
}

# Calcola il numero di righe necessarie per il layout del foglio
num_righe <- ceiling(num_variabili / 8)

# Visualizza tutti i grafici in un unico grande foglio
grid.arrange(grobs = plot_list, ncol = 8, nrow = num_righe)


########################### 

# Total numebers of variables
num_variabili <- ncol(extracted)

# Numero di fogli necessari
num_fogli <- ceiling(num_variabili / 6)

# Ciclo per creare i fogli di grafici
for (j in 1:num_fogli) {
  # Calcola gli indici delle variabili per questo foglio
  start_index <- (j - 1) * 6 + 1
  end_index <- min(j * 6, num_variabili)
  
  # Calcola il numero di righe e colonne per il layout del foglio
  num_colonne <- min(end_index - start_index + 1, 3)
  num_righe <- ceiling((end_index - start_index + 1) / num_colonne)
  
  # Se il numero di variabili nel foglio è inferiore a 6, imposta il numero di righe e colonne di conseguenza
  if (end_index - start_index + 1 < 6) {
    num_colonne <- end_index - start_index + 1
    num_righe <- 1
  }
  
  # Crea una lista per memorizzare i grafici di questo foglio
  plot_list <- list()
  
  for (i in start_index:end_index) {
    # Crea un nuovo grafico utilizzando ggplot2 e lo aggiunge alla lista
    plot_list[[i - start_index + 1]] <- ggplot(data = extracted, aes(x = .data[[names(extracted)[i]]])) +
      geom_histogram(aes(y = after_stat(density)), bins = 30, fill = "lightgray", color = "black") +
      geom_density(color = "red") +
      labs(x = "Value", y = "Density", title = names(extracted)[i])
  }
  
  # Visualizza tutti i grafici di questo foglio utilizzando grid.arrange
  if(length(plot_list) > 0){
    grid.arrange(grobs = plot_list, ncol = num_colonne, nrow = num_righe)
  }
}