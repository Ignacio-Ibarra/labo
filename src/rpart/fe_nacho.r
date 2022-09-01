rm( list=ls() )  
gc()   

require("data.table")
require("rpart")
require("parallel")
require("caret")
require(ggplot2)
require(ggridges)
require(readxl)


setwd("~/Desktop/EyF 2022")  


df <- read_excel("./TareasHogar/HT_anal.xlsx", sheet="Hoja4")


#Mejores param. 
MAX_DEPTH <- 6
MIN_split <- 600
CP <- -1
MIN_BUCKET <- 300

#------------------------------------------------------------------------------
#particionar agrega una columna llamada fold a un dataset que consiste en una particion estratificada segun agrupa
# particionar( data=dataset, division=c(70,30), agrupa=clase_ternaria, seed=semilla)   crea una particion 70, 30 

particionar  <- function( data,  division, agrupa="",  campo="fold", start=1, seed=NA )
{
  if( !is.na(seed) )   set.seed( seed )
  
  bloque  <- unlist( mapply(  function(x,y) { rep( y, x )} ,   division,  seq( from=start, length.out=length(division) )  ) )  
  
  data[ , (campo) :=  sample( rep( bloque, ceiling(.N/length(bloque))) )[1:.N],
        by= agrupa ]
}
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------

#cargo los datos
dataset  <- fread("./datasets/competencia1_2022.csv")

#trabajo solo con los datos con clase, es decir 202101
dataset  <- dataset[ clase_ternaria!= "" ]

#particiono estratificadamente el dataset
#Cambiar por la primer semilla de cada uno !
particionar( dataset, division=c(7,3), agrupa="clase_ternaria", seed= 635837 )  #Cambiar por la primer semilla de cada uno !


param_basicos  <- list( "cp"=         CP,  #complejidad minima
                        "minsplit"=  MIN_split,  #minima cantidad de registros en un nodo para hacer el split
                        "minbucket"=  MIN_BUCKET,  #minima cantidad de registros en una hoja
                        "maxdepth"=  MAX_DEPTH ) #profundidad máxima del arbol

#genero el modelo
modelo  <- rpart("clase_ternaria ~ .",     #quiero predecir clase_ternaria a partir del resto
                 data= dataset[ fold==1],  #fold==1  es training,  el 70% de los datos
                 xval= 0,
                 control=  param_basicos )  #aqui van los parametros

require(rpart.plot)

options(repr.plot.width=2500, repr.plot.height=2500)  #para que los gráficos me salgan legibles
png("./TareasHogar/arbol.png", width = 2000, height = 2000)
prp(modelo, extra=101, digits=5, branch=1, type=4, varlen=0, faclen=0, tweak=1.3)
dev.off()


dataset <- dataset[  , ctrx_quarter_20 :=  as.integer(ctrx_quarter<20)] 
dataset <- dataset[  , mcuentas_saldo_1513 :=  as.integer(mcuentas_saldo<-1513.9)] 
dataset <- dataset[  , ctrx_quarter_41 :=  as.integer(ctrx_quarter<41)]
dataset <- dataset[  , mcaja_ahorro_2593 :=  as.integer(mcaja_ahorro<2593.2)]
dataset <- dataset[  , mpasivos_margen_73 :=  as.integer(mpasivos_margen<73.965)]