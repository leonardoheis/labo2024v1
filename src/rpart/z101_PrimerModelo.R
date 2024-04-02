# Arbol elemental con libreria  rpart
# Debe tener instaladas las librerias  data.table  ,  rpart  y  rpart.plot

# cargo las librerias que necesito
require("data.table")
require("rpart")
require("rpart.plot")

# Aqui se debe poner la carpeta de la materia de SU computadora local
#setwd("C:\\Users\\leona\\Desktop\\MCD\\Laboratorio_de_Implementacion_I\\") # Establezco el Working Directory
setwd("C:/Users/leona/Desktop/MCD/Laboratorio_de_Implementacion_I/")


# cargo el dataset
#dataset <- fread("./datasets/dataset_pequeno.csv")
dataset <- fread("./datasets/dataset_pequeno.csv")

dtrain <- dataset[foto_mes == 202107] # defino donde voy a entrenar
dapply <- dataset[foto_mes == 202109] # defino donde voy a aplicar el modelo

#table(is.na(dtrain))

# genero el modelo,  aqui se construye el arbol
# quiero predecir clase_ternaria a partir de el resto de las variables
modelo <- rpart(
        formula = "clase_ternaria ~ .",
        data = dtrain, # los datos donde voy a entrenar
        xval = 0,
        cp = -1, #-0.3, # esto significa no limitar la complejidad de los splits
        minsplit = 700,#0, # minima cantidad de registros para que se haga el split -- min_split >= 2 * min_bucket
        minbucket = 175,#1, # tamaño minimo de una hoja
        maxdepth = 6 #3
) # profundidad maxima del arbol


# grafico el arbol
prp(modelo,
        extra = 101, digits = -5,
        branch = 1, type = 4, varlen = 0, faclen = 0
)


# aplico el modelo a los datos nuevos
prediccion <- predict(
        object = modelo,
        newdata = dapply,
        type = "prob"
)

# prediccion es una matriz con TRES columnas,
# llamadas "BAJA+1", "BAJA+2"  y "CONTINUA"
# cada columna es el vector de probabilidades

# agrego a dapply una columna nueva que es la probabilidad de BAJA+2
dapply[, prob_baja2 := prediccion[, "BAJA+2"]]

# solo le envio estimulo a los registros
#  con probabilidad de BAJA+2 mayor  a  1/40
dapply[, Predicted := as.numeric(prob_baja2 > 1 / 40)]

# genero el archivo para Kaggle
# primero creo la carpeta donde va el experimento
if (!file.exists("./exp/")) {
        dir.create("./exp/")
}

if (!file.exists("./exp/KA2001")) {
        dir.create("./exp/KA2001")
}

# solo los campos para Kaggle
timestamp <- format(Sys.time(), "%Y%m%d%H%M%S")  # Get current timestamp
#sequential_value <- 1  # Set initial sequential value
# Check if a file name exists in ./exp/KA2001
if (file.exists("./exp/KA2001")) {        # Get the list of files in the directory
        #print("hay")
        files <- list.files(path = "./exp/KA2001", pattern = "K101_\\d{14}_\\d{3}\\.csv", full.names = TRUE)
        
        # Sort the files by creation time
        files <- sort(files, decreasing = TRUE)
        
        # Get the last file created
        last_file <- files[1]

                # Extract the last three digits from the file name
        last_digits <- substring(last_file, nchar(last_file) - 6, nchar(last_file) - 4)
        
        # Convert the digits to numeric
        last_digits <- as.numeric(last_digits)
        
        # Increment the sequential value for the next run
        sequential_value <- last_digits + 1
} else {
        # If the directory doesn't exist, set the sequential value to 1
        #print("no hay")
        sequential_value <- 1
}

# Generate the unique filename
sequential_value
filename <- paste0("./exp/KA2001/K101_", timestamp, "_", sprintf("%03d", sequential_value), ".csv")

# Increment the sequential value for the next run
sequential_value <- sequential_value + 1

# Write the data to the file
fwrite(dapply[, list(numero_de_cliente, Predicted)],
                         file = filename,
                         sep = ",")

