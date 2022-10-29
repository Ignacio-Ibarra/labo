require(data.table)
require(dplyr)
require(gridExtra)
setwd("~/buckets/b1/")
#Parametros del script
PARAM  <- list()
PARAM$experimento <- "ESTACIONALIDAD"
PARAM$dataset <- "./datasets/competencia3_2022.csv.gz"
PARAM$future <- c( 202107)

datos <- fread(PARAM$dataset)

estacio <- datos[!foto_mes %in% c(202106,202107), .(cant = .N), by = .(foto_mes,clase_ternaria)]

estacio <- estacio %>% group_by(foto_mes) %>% mutate(prop=cant/sum(cant))

g1 <- ggplot(estacio, aes(x=foto_mes, y=cant))+
  geom_line()+
  facet_wrap(vars(clase_ternaria), scale = "free_y", nrow=1)

g2 <- ggplot(estacio, aes(x=foto_mes, y=prop))+
  geom_line()+
  facet_wrap(vars(clase_ternaria), scale = "free_y", nrow=1)

grid.arrange(g1, g2)