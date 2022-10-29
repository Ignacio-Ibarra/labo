require(data.table)
require(dplyr)
require(gridExtra)
require(ggplot2)
require(lubridate)

setwd("~/buckets/b1/")
#Parametros del script
PARAM  <- list()
PARAM$experimento <- "ESTACIONALIDAD"
PARAM$dataset <- "./datasets/competencia3_2022.csv.gz"
PARAM$future <- c( 202107)

datos <- fread(PARAM$dataset)

estacio <- datos[!foto_mes %in% c(202106,202107), .(cant = .N), by = .(foto_mes,clase_ternaria)]

#creo la carpeta donde va el experimento
dir.create( paste0( "./exp/", PARAM$experimento, "/"), showWarnings = FALSE )
setwd(paste0( "./exp/", PARAM$experimento, "/"))

fwrite(estacio, 
      file = "estacionalidad.csv",
      logical01= TRUE,
      sep= ",")

estacio <- estacio %>% group_by(ym(foto_mes)) %>% mutate(prop=cant/sum(cant))


g1 <- ggplot(estacio, aes(x=ym(foto_mes), y=cant))+
  geom_line()+
  facet_wrap(vars(clase_ternaria), scale = "free_y", nrow=1)+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  theme_light()

g2 <- ggplot(estacio, aes(x=ym(foto_mes), y=prop))+
  geom_line()+
  facet_wrap(vars(clase_ternaria), scale = "free_y", nrow=1)+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  theme_light()

png(filename = "estacionalidad.png")
grid.arrange(g1, g2)
dev.off()