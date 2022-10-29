require(data.table)
require(dplyr)
setwd("~/buckets/b1/")
#Parametros del script
PARAM  <- list()
PARAM$experimento <- "ESTACIONALIDAD"
PARAM$dataset <- "./datasets/competencia3_2022.csv.gz"
PARAM$future <- c( 202107)

datos <- fread(PARAM$dataset)

estacio <- datos[!foto_mes %in% c(202106,202107), .(cant = .N), by = .(foto_mes,clase_ternaria)]

estacio %>% group_by(foto_mes) %>% mutate(prop=cant/sum(cant))
