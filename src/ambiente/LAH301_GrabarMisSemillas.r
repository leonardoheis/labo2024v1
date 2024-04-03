# Este script almacena definitivamente sus cinco semillas
# en el bucket, de forma que NO deba cargarlas en cada script
rm(list = ls()) # Borro todos los objetos
gc() # Garbage Collection

require("data.table")

# reemplazar aqui por SUS semillas 
mis_semillas <- c(292351, 860369, 275641, 717917, 570719)

tabla_semillas <- as.data.table(list(semilla = mis_semillas))

fwrite(tabla_semillas,
    file = "~/buckets/b1/datasets/mis_semillas.txt",
    sep = "\t"
)
