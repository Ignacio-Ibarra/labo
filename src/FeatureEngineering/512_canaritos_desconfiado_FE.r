#limpio la memoria
rm( list=ls() )  #remove all objects
gc()             #garbage collection

require("data.table")
require("rpart")
require("rpart.plot")

setwd( "~/buckets/b1/" )

#cargo el dataset
dataset  <- fread( "./exp/FE7110/dataset_7110.csv.gz")

#uso esta semilla para los canaritos
set.seed(633337)

#agrego una variable canarito, random distribucion uniforme en el intervalo [0,1]
dataset[ ,  canarito1 :=  runif( nrow(dataset) ) ]

#agrego los siguientes canaritos
for( i in 13:100 ) dataset[ , paste0("canarito", i ) :=  runif( nrow(dataset)) ]


#Primero  veo como quedan mis arboles
  modelo  <- rpart(formula= "clase_ternaria ~ . -mcomisiones_mantenimiento -Visa_mpagado",
                 data= dataset[ foto_mes==202103 ,],
                 model= TRUE,
                 xval= 0,
                 cp= -0.01,
                 minsplit= 200,
                 minbucket= 100,
                 maxdepth= 6)


pdf(file = "./exp/FE7110/arbol_canaritos_desconfiado_nacho.pdf", width=28, height=4)
prp(modelo, extra=101, digits=5, branch=1, type=4, varlen=0, faclen=0)
dev.off()

