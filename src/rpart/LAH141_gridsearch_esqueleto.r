# esqueleto de grid search
# se espera que los alumnos completen lo que falta
#   para recorrer TODOS cuatro los hiperparametros

rm(list = ls()) # Borro todos los objetos
gc() # Garbage Collection

require("data.table")
require("rpart")
require("parallel")

PARAM <- list()
# reemplazar por las propias semillas
PARAM$semillas <- c(292351, 860369, 275641, 717917, 570719)

#------------------------------------------------------------------------------
# particionar agrega una columna llamada fold a un dataset
#  que consiste en una particion estratificada segun agrupa
# particionar( data=dataset, division=c(70,30), agrupa=clase_ternaria, seed=semilla)
#   crea una particion 70, 30

particionar <- function(data, division, agrupa = "", campo = "fold", start = 1, seed = NA) {
  if (!is.na(seed)) set.seed(seed)

  bloque <- unlist(mapply(function(x, y) {
    rep(y, x)
  }, division, seq(from = start, length.out = length(division))))

  data[, (campo) := sample(rep(bloque, ceiling(.N / length(bloque))))[1:.N],
    by = agrupa
  ]
}
#------------------------------------------------------------------------------

ArbolEstimarGanancia <- function(semilla, param_basicos) {
  # particiono estratificadamente el dataset
  particionar(dataset, division = c(7, 3), agrupa = "clase_ternaria", seed = semilla)

  # genero el modelo
  # quiero predecir clase_ternaria a partir del resto
  modelo <- rpart("clase_ternaria ~ .",
    data = dataset[fold == 1], # fold==1  es training,  el 70% de los datos
    xval = 0,
    control = param_basicos
  ) # aqui van los parametros del arbol

  # aplico el modelo a los datos de testing
  prediccion <- predict(modelo, # el modelo que genere recien
    dataset[fold == 2], # fold==2  es testing, el 30% de los datos
    type = "prob"
  ) # type= "prob"  es que devuelva la probabilidad

  # prediccion es una matriz con TRES columnas,
  #  llamadas "BAJA+1", "BAJA+2"  y "CONTINUA"
  # cada columna es el vector de probabilidades


  # calculo la ganancia en testing  qu es fold==2
  ganancia_test <- dataset[
    fold == 2,
    sum(ifelse(prediccion[, "BAJA+2"] > 0.025,
      ifelse(clase_ternaria == "BAJA+2", 117000, -3000),
      0
    ))
  ]

  # escalo la ganancia como si fuera todo el dataset
  ganancia_test_normalizada <- ganancia_test / 0.3

  return(ganancia_test_normalizada)
}
#------------------------------------------------------------------------------

ArbolesMontecarlo <- function(semillas, param_basicos) {
  # la funcion mcmapply  llama a la funcion ArbolEstimarGanancia
  #  tantas veces como valores tenga el vector  PARAM$semillas
  ganancias <- mcmapply(ArbolEstimarGanancia,
    semillas, # paso el vector de semillas
    MoreArgs = list(param_basicos), # aqui paso el segundo parametro
    SIMPLIFY = FALSE,
    mc.cores = 5 # en Windows este valor debe ser 1
  )

  ganancia_promedio <- mean(unlist(ganancias))

  return(ganancia_promedio)
}
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------

# Aqui se debe poner la carpeta de la computadora local
setwd("~/buckets/b1/") # Establezco el Working Directory
# setwd("~\\..\\")
# cargo los datos

# cargo los datos
dataset <- fread("./datasets/dataset_pequeno.csv")
# dataset <- fread("./Desktop/MCD/Laboratorio_de_Implementacion_I/datasets/dataset_pequeno.csv")

# trabajo solo con los datos con clase, es decir 202107
dataset <- dataset[clase_ternaria != ""]

# genero el archivo para Kaggle
# creo la carpeta donde va el experimento
# HT  representa  Hiperparameter Tuning
dir.create("./exp/", showWarnings = FALSE)
dir.create("./exp/HT2020/", showWarnings = FALSE)
imestamp <- format(Sys.time(), "%Y%m%d%H%M%S") 
archivo_salida <- paste0("./exp/HT2020/gridsearch", timestamp,".txt")
# dir.create("./Desktop/MCD/Laboratorio_de_Implementacion_I/exp/", showWarnings = TRUE)
# dir.create("./Desktop/MCD/Laboratorio_de_Implementacion_I/exp/HT2020/", showWarnings = TRUE)
# archivo_salida <- "./Desktop/MCD/Laboratorio_de_Implementacion_I/exp/HT2020/gridsearch.txt"

# genero la data.table donde van los resultados del Grid Search
tb_grid_search <- data.table(
  max_depth = integer(),
  min_split = integer(),
  cp = integer(),
  min_bucket = integer(),
  ganancia_promedio = numeric()
)

# Generate all combinations of hyperparameters
# hyperparams <- expand.grid(
#    vcp = c(-1, -0.5, -0.3),
#    vmax_depth = c(4, 6, 10),
#    vmin_split = c(800, 600, 400),
#    vmin_bucket = c(800, 600, 400)
# )

# vmin_split <- c(200, 400, 450, 500, 550, 600, 800)
# hyperparams <- expand.grid(
#   vcp = c(-1, -0.9, -0.8, -0.6, -0.5, -0.2, 0),
#   vmax_depth = c(2, 5, 6, 10, 14, 20, 25, 30),
#   vmin_split = vmin_split,
#   vmin_bucket = c(vmin_split/2 ,  vmin_split/3,  vmin_split/4, vmin_split/5,  10, 5, 2, 1 )
# )
#
# # Calculate average gain for each combination of hyperparameters
# results <- lapply(1:nrow(hyperparams), function(i) {
#     param_basicos <- list(
#         "cp" = hyperparams$vcp[i],
#         "minsplit" = hyperparams$vmin_split[i],
#         "minbucket" = hyperparams$vmin_bucket[i],
#         "maxdepth" = hyperparams$vmax_depth[i]
#     )
#     ganancia_promedio <- ArbolesMontecarlo(PARAM$semillas, param_basicos)
#     ganancia_promedio
#     c(param_basicos, ganancia_promedio)
# })

# for (vmax_depth in c(2, 4, 5, 6, 8, 10)) {
#   for (vmin_split in c(1000, 800, 600, 400, 200, 100, 50, 20, 10)) {
#     for (vcp in c(-1, -0.8, -0.6, -0.5, -0.2, 0)) {
#       for (vmin_bucket in c(vmin_split/2 ,  vmin_split/3,  vmin_split/4, vmin_split/5, 20, 10, 5, 2, 1 )) {
for (vmax_depth in c(5, 6)) {
  for (vmin_split in c(800, 750, 700, 650, 600, 550, 500)) {
    for (vcp in c(-1, -0.5, 0)) {
      for (vmin_bucket in c(vmin_split / 2, vmin_split / 3, vmin_split / 4, vmin_split / 5, 20, 10, 5, 2, 1)) {
        # vminsplit  minima cantidad de registros en un nodo para hacer el split
        param_basicos <- list(
          "cp" = vcp, #-0.5, # complejidad minima
          "minsplit" = vmin_split,
          "minbucket" = vmin_bucket, # 5, # minima cantidad de registros en una hoja
          "maxdepth" = vmax_depth
        ) # profundidad máxima del arbol

        # Un solo llamado, con la semilla 17
        ganancia_promedio <- ArbolesMontecarlo(PARAM$semillas, param_basicos)

        # agrego a la tabla
        tb_grid_search <- rbindlist(
          list(
            tb_grid_search,
            list(vmax_depth, vmin_split, vcp, vmin_bucket, ganancia_promedio)
          )
        )
      }
    }
  }
}

# escribo la tabla a disco en cada vuelta del loop mas externo
Sys.sleep(2) # espero un par de segundos

fwrite(tb_grid_search,
  file = archivo_salida,
  sep = "\t"
)


# Combine the results into a single data.table
# tb_grid_search <- rbindlist(results)
# Sys.sleep(2)  # espero un par de segundos
# Write the final data.table to disk
# fwrite(tb_grid_search, file = archivo_salida, sep = "\t")
