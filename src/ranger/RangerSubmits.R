
#limpio la memoria
rm( list=ls() )  #Borro todos los objetos
gc()   #Garbage Collection

require("data.table")
require("ranger")
require("randomForest") 

setwd("~/buckets/b1/")
# setwd("~/Desktop/EyF 2022")


experimentos <- fread("./exp/HT6330/HT6330.txt")

topN <- 5
correr <- experimentos[order(-ganancia), head(.SD, topN)][, .(num.trees, max.depth, min.node.size, mtry, iteracion)]

#cargo los datos donde entreno
dataset  <- fread("./datasets/competencia2_2022.csv.gz", stringsAsFactors= TRUE) 

#paso a trabajar con clase binaria POS={BAJA+2}   NEG={BAJA+1, CONTINUA}
dataset[ , clase_binaria := as.factor(ifelse( clase_ternaria=="BAJA+2", "POS", "NEG" )) ]
dataset[ , clase_ternaria := NULL ]  #elimino la clase_ternaria, ya no la necesito


#imputo los nulos, ya que ranger no acepta nulos
#Leo Breiman, ¿por que le temias a los nulos?
dataset  <- na.roughfix( dataset[ foto_mes %in% c( 202103, 202105 ) ] )

dtrain  <- dataset[ foto_mes == 202103 ]
dapply  <- dataset[ foto_mes == 202105 ]


fit.predict <- function(param.list, train.set, test.set){
  
  
  iteracion <- param.list$iteracion
  
  modelo  <- ranger( formula= "clase_binaria ~ .",
                     data=  train.set, 
                     probability=   TRUE,  #para que devuelva las probabilidades
                     num.trees=     param.list$num.trees,
                     mtry=          param.list$mtry,
                     min.node.size= param.list$min.node.size,
                     max.depth=     param.list$max.depth
                     #,class.weights= c( 1,40, 1)  #siguiendo con la idea de Maite San Martin
                  )
  
  cat("Ya entrenó iteracion: ", iteracion)
  
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
  
  for( corte  in  c( 8500, 9000, 9500, 10000) )
  {
    #le envio a los  corte  mejores,  de mayor probabilidad de prob_SI
    dfinal[ , Predicted := 0L ]
    dfinal[ 1:corte , Predicted := 1L ]
    
    
    fwrite( dfinal[ , list(numero_de_cliente, Predicted) ], #solo los campos para Kaggle
            file= paste0( "./exp/", kaggle.folder,"/",kaggle.folder,"_it_",iteracion,"_corte_", corte, ".csv"),
    sep=  "," )
  }
  
}

lapply(1:topN, function(x) fit.predict(correr[x], dtrain, dapply))
