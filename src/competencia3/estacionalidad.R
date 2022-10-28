require(data.table)
setwd("~/buckets/b1/")
#Parametros del script
PARAM  <- list()
PARAM$experimento <- "ESTACIONALIDAD"
PARAM$dataset <- "./datasets/competencia3_2022.csv.gz"
PARAM$future <- c( 202107)

datos <- fread(PARAM$dataset)

unique(datos$foto_mes)