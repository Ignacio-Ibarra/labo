# Tengo un dataset muy grande con una columna que indica el mes
# y otra columna que indica la clase. 
# quiero obtener un dataset más pequeño, de prueba, que pueda usar en local
# para ir probando. 
# el objetivo es que tenga la misma proporción de filas correspondientes a cada mes
# y mismas filas correspondientes a cada clase. 

require(data.table)
require(dplyr)

setwd("~/labo")
data <- fread("../buckets/b1/datasets/competencia2_2022.csv.gz")

dplyr_stratified <- function(df, percent, ...){
  columns<-enquos(...)
  out<-df %>% group_by(!!!columns)  %>% slice( sample(1:n(), percent*n())) 
}

muestra<-dplyr_stratified(data, 0.5, foto_mes, clase_ternaria)

fwrite(muestra, "../buckets/b2/muestra_comp2.csv", row.names = F)



