#limpio la memoria
rm( list=ls() )  #remove all objects
gc()             #garbage collection


require("data.table")
require("ggplot2")

setwd("~/Desktop/EyF 2022") 


experimentos <- fread("./exp/HT4110/HT4110.txt")


correr <- experimentos[order(-ganancia), head(.SD, 5)][, .(cp, minsplit, minbucket, maxdepth, iteracion)]


#cargo el dataset
dataset  <- fread("./datasets/competencia1_2022.csv")

#creo la clase_binaria SI={ BAJA+1, BAJA+2 }    NO={ CONTINUA }
dataset[ foto_mes==202101, 
         clase_binaria :=  ifelse( clase_ternaria=="CONTINUA", "NO", "SI" ) ]

dtrain  <- dataset[ foto_mes==202101 ]  #defino donde voy a entrenar
dapply  <- dataset[ foto_mes==202103 ]  #defino donde voy a aplicar el modelo


fit.predict <- function(param.list, train.set, test.set){
  
  modelo <- rpart(formula =  "clase_binaria ~ . -clase_ternaria",
                  data = train.set.
                  control = param.list[, -"iteracion", with=F])
  
  prediccion  <- predict(modelo,   
                         test.set,  
                         type= "prob")
  
  #agrego a dapply una columna nueva que es la probabilidad de BAJA+2
  dfinal  <- copy( test.set[ , list(numero_de_cliente) ] )
  dfinal[ , prob_SI := prediccion[ , "SI"] ]
  
  
  # por favor cambiar por una semilla propia
  # que sino el Fiscal General va a impugnar la prediccion
  set.seed(635837)  
  dfinal[ , azar := runif( nrow(test.set) ) ]
  
  # ordeno en forma descentente, y cuando coincide la probabilidad, al azar
  setorder( dfinal, -prob_SI, azar )
  
  dia.mes <- format(Sys.Date(),"%d%m")
  dir.create( "./exp/" )
  kaggle.folder <- paste0("KA", dia.mes)
  dir.create( paste0("./exp/", kaggle.folder) )
  iteracion <- param.list$iteracion
  
  for( corte  in  c( 8500, 9000, 9500) )
  {
    #le envio a los  corte  mejores,  de mayor probabilidad de prob_SI
    dfinal[ , Predicted := 0L ]
    dfinal[ 1:corte , Predicted := 1L ]
    
    
    fwrite( dfinal[ , list(numero_de_cliente, Predicted) ], #solo los campos para Kaggle
            file= paste0( "./exp/", kaggle.folder),"/",kaggle.folder,"_it_",iteracion,"_corte_", corte, ".csv"),
            sep=  "," )
  }
  
}

mcmapply( fit.predict, 
          correr, 
          MoreArgs = list(dtrain, dapply), 
          SIMPLIFY= FALSE,
          mc.cores= 2) 
