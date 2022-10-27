
# para correr el Google Cloud
#   8 vCPU
#  64 GB memoria RAM
# 256 GB espacio en disco

# son varios archivos, subirlos INTELIGENTEMENTE a Kaggle

#limpio la memoria
rm( list=ls() )  #Borro todos los objetos
gc()   #Garbage Collection

#install.packages("gtools")
require("data.table")
require("lightgbm")
require("gtools")
require("stringr")

setwd("~/buckets/b1/")
# setwd("~/Desktop/EyF 2022")


bayesian.opt.output <- "./exp/HT3009/HT3009.txt"

experimentos <- fread(bayesian.opt.output)
cantidad <- min(c(round(0.3*nrow(experimentos)),20))

cols <-  c("num_iterations","learning_rate","feature_fraction","min_data_in_leaf","num_leaves","lambda_l1","lambda_l2", "iteracion")
cols <-  c("num_iterations","learning_rate","feature_fraction","min_data_in_leaf","num_leaves", "iteracion")

correr <- experimentos[order(-ganancia), head(.SD, cantidad)][,..cols]


dia.mes <- format(Sys.Date(),"%d%m")
dir.create( "./exp/" )
kaggle.folder <- paste0("KA", dia.mes)

FIXED_PARAM <- list()
FIXED_PARAM$experimento  <- kaggle.folder
# FIXED_PARAM$experimento  <- "KA3009"
FIXED_PARAM$input$dataset       <- "./exp/FE2809/FE2809_dataset.csv.gz" #contiene Feat.Eng
# FIXED_PARAM$input$dataset       <- "./datasets/datasets_muestra25_comp2.csv" #muestra aleatoria en local de prueba
FIXED_PARAM$input$training      <- c( 202103 )
FIXED_PARAM$input$future        <- c( 202105 )
# PARAM$finalmodel$semilla           <- 635837



#cargo el dataset donde voy a entrenar
dataset  <- fread(FIXED_PARAM$input$dataset, stringsAsFactors= TRUE)
#paso la clase a binaria que tome valores {0,1}  enteros
#set trabaja con la clase  POS = { BAJA+1, BAJA+2 } 
#esta estrategia es MUY importante
dataset[ , clase01 := ifelse( clase_ternaria %in%  c("BAJA+2","BAJA+1"), 1L, 0L) ]

#--------------------------------------

#los campos que se van a utilizar
campos_buenos  <- setdiff( colnames(dataset), c("clase_ternaria","clase01") )


folder.path <- paste0("./exp/", FIXED_PARAM$experimento, "/" )
dir.create( folder.path, showWarnings = FALSE )

#--------------------------------------

#establezco donde entreno
dataset[ , train  := 0L ]
dataset[ foto_mes %in% FIXED_PARAM$input$training, train  := 1L ]

#dejo los datos en el formato que necesita LightGBM
dtrain  <- lgb.Dataset( data= data.matrix(  dataset[ train==1L, campos_buenos, with=FALSE]),
                        label= dataset[ train==1L, clase01] )

#aplico el modelo a los datos sin clase
dapply  <- dataset[ foto_mes== FIXED_PARAM$input$future ]

fit.predict = function(param.list, train.set, test.set){

iteracion <- param.list$iteracion  
#genero el modelo
#estos hiperparametros  salieron de una laaarga Optmizacion Bayesiana
modelo  <- lgb.train( data= train.set,
                      param= list( objective=          "binary",
                                   max_bin=            31, #lo mantengo fijo
                                   learning_rate=      param.list$learning_rate,
                                   num_iterations=     param.list$num_iterations,
                                   num_leaves=         param.list$num_leaves,
                                   min_data_in_leaf=   param.list$min_data_in_leaf,
                                   feature_fraction=   param.list$feature_fraction,
                                   lambda_l1 =         param.list$lambda_l1,
                                   lambda_l2 =         param.list$lambda_l2,
                                   seed=               635837,
                                   feature_pre_filter= FALSE
                      )
)





#aplico el modelo a los datos nuevos
prediccion  <- predict( modelo, 
                        data.matrix( test.set[, campos_buenos, with=FALSE ])                                 )

#genero la tabla de entrega
tb_entrega  <-  test.set[ , list( numero_de_cliente, foto_mes ) ]
tb_entrega[  , prob := prediccion ]

# #grabo las probabilidad del modelo
# fwrite( tb_entrega,
#         file= "prediccion.txt",
#         sep= "\t" )

#ordeno por probabilidad descendente
setorder( tb_entrega, -prob )


#genero archivos con los  "envios" mejores
#deben subirse "inteligentemente" a Kaggle para no malgastar submits
cortes <- seq( 8500, 9000, by=125 )
for( envios  in  cortes )
{
  tb_entrega[  , Predicted := 0L ]
  tb_entrega[ 1:envios, Predicted := 1L ]
  
  fwrite( tb_entrega[ ,list(numero_de_cliente, Predicted)], 
          file= paste0(folder.path, FIXED_PARAM$experimento,"_", iteracion, "_", envios, ".csv" ),
          sep= "," )
}


}

#Esto sirve para poder ejecurtar este script y que solo corra lo que falta. 
files.in <- list.files(folder.path, pattern = "*.csv")
all.iters <- correr$iteracion
iters.runned  <- as.integer(unique(str_extract(files.in, "(?<=\\_)[0-9]+(?=\\_)")))
falta.correr <- setdiff(all.iters, iters.runned)

lapply(falta.correr, function(iter) fit.predict(correr[iteracion==iter, ], dtrain, dapply))

#--------------------------------------

quit( save= "no" )


