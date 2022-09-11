
#limpio la memoria
rm( list=ls() )  #remove all objects
gc()             #garbage collection

require("data.table")
require("rlist")
require(readODS)
require("rpart")
require("parallel")

#paquetes necesarios para la Bayesian Optimization
require("DiceKriging")
require("mlrMBO")

#aqui deben ir SUS semillas, se usan para  1-Repeated  (5-Fold Cross Validation)
ksemilla_azar  <- c(450473)


#Defino la  Optimizacion Bayesiana

kBO_iter  <- 150   #cantidad de iteraciones de la Optimizacion Bayesiana

hs  <- makeParamSet(
  makeNumericParam("cp"       , lower=  -1.0, upper=    0.1),
  makeNumericParam("minsplit" , lower=   100,   upper= 3000 ),
  makeNumericParam("minbucket", lower=   200,   upper= 1000 ),
  makeIntegerParam("maxdepth" , lower=   6L,  upper=   20L),  #la letra L al final significa ENTERO
  forbidden = quote( minbucket > 0.5*minsplit ) )             # minbuket NO PUEDE ser mayor que la mitad de minsplit


#------------------------------------------------------------------------------
#graba a un archivo los componentes de lista
#para el primer registro, escribe antes los titulos

loguear  <- function( reg, arch=NA, folder="./work/", ext=".txt", verbose=TRUE )
{
  archivo  <- arch
  if( is.na(arch) )  archivo  <- paste0( folder, substitute( reg), ext )
  
  if( !file.exists( archivo ) )  #Escribo los titulos
  {
    linea  <- paste0( "fecha\t", 
                      paste( list.names(reg), collapse="\t" ), "\n" )
    
    cat( linea, file=archivo )
  }
  
  linea  <- paste0( format(Sys.time(), "%Y%m%d %H%M%S"),  "\t",     #la fecha y hora
                    gsub( ", ", "\t", toString( reg ) ),  "\n" )
  
  cat( linea, file=archivo, append=TRUE )  #grabo al archivo
  
  if( verbose )  cat( linea )   #imprimo por pantalla
}
#------------------------------------------------------------------------------
#particionar agrega una columna llamada fold a un dataset que consiste en una particion estratificada segun agrupa
# particionar( data=dataset, division=c(70,30), agrupa=clase_ternaria, seed=semilla)   crea una particion 70, 30 
# particionar( data=dataset, division=c(1,1,1,1,1), agrupa=clase_ternaria, seed=semilla)   divide el dataset en 5 particiones

particionar  <- function( data, division, agrupa="", campo="fold", start=1, seed=NA )
{
  if( !is.na( seed)  )   set.seed( seed )
  
  bloque  <- unlist( mapply(  function(x,y) { rep( y, x ) }, division, seq( from=start, length.out=length(division) )  ) )
  
  data[ , (campo) :=  sample( rep( bloque, ceiling(.N/length(bloque))) )[1:.N],
        by= agrupa ]
}
#------------------------------------------------------------------------------
#fold_test  tiene el numero de fold que voy a usar para testear, entreno en el resto de los folds
#param tiene los hiperparametros del arbol

ArbolSimple  <- function( fold_test, data, param )
{
  param2 <- param
  #param2$minsplit   <- as.integer( round( 2^param$minsplit ) )
  #param2$minbucket  <- as.integer( round( 2^param$minbucket ) )
  
  #genero el modelo
  modelo  <- rpart("clase_binaria ~ .  -Visa_mpagado -clase_ternaria",
                   data= data[ fold != fold_test, ],  #entreno en todo MENOS el fold_test que uso para testing
                   xval= 0,
                   control= param2 )
  
  #aplico el modelo a los datos de testing
  prediccion  <- predict( modelo, 
                          data[ fold==fold_test, ],  #aplico el modelo sobre los datos de testing
                          type= "prob")   #quiero que me devuelva probabilidades
  
  #En el 1er cuatrimestre del Tercer Año de la Maestria se explicaran las siguientes 12 lineas
  dtest <- copy( data[ fold==fold_test , list( clase_ternaria )] )
  dtest[ , pred := prediccion[ ,"SI"] ]
  dtest[ , azar := runif( nrow( dtest ) ) ]
  setorder(  dtest, -pred, azar )
  
  dtest[ , gan :=  ifelse( clase_ternaria=="BAJA+2", 78000, -2000 ) ]
  dtest[ , gan_acum := cumsum( gan ) ]
  
  #calculo la ganancia
  dtest2   <- dtest[ (1:100)*100,  ]
  idx_max  <- which.max( dtest2$gan_acum ) 
  ganancia_testing  <- dtest2[ (idx_max-1):(idx_max+1),  mean(gan_acum) ]
  
  
  rm( dtest )
  rm( dtest2 )
  
  return( ganancia_testing )  #esta es la ganancia sobre el fold de testing, NO esta normalizada
}
#------------------------------------------------------------------------------

ArbolesCrossValidation  <- function( semilla, data, param, qfolds, pagrupa )
{
  divi  <- rep( 1, qfolds )  # generalmente  c(1, 1, 1, 1, 1 )  cinco unos
  
  particionar( data, divi, seed=semilla, agrupa=pagrupa )  #particiono en dataset en folds
  
  ganancias  <- mcmapply( ArbolSimple, 
                          seq(qfolds), # 1 2 3 4 5
                          MoreArgs= list( data, param), 
                          SIMPLIFY= FALSE,
                          mc.cores= 5 )   #debe ir 1 si es Windows
  
  data[ , fold := NULL ]
  
  #devuelvo la primer ganancia y el promedio
  ganancia_promedio  <- mean( unlist( ganancias ) )   #promedio las ganancias
  ganancia_promedio_normalizada  <- ganancia_promedio * qfolds  #aqui normalizo la ganancia
  
  gc()
  
  return( ganancia_promedio_normalizada )
}
#------------------------------------------------------------------------------
#esta funcion solo puede recibir los parametros que se estan optimizando
#el resto de los parametros, lamentablemente se pasan como variables globales

EstimarGanancia  <- function( x )
{
  GLOBAL_iteracion  <<-  GLOBAL_iteracion + 1
  
  xval_folds  <- 5
  vganancias <- mcmapply( ArbolesCrossValidation,
                          ksemilla_azar,
                          MoreArgs= list ( dtrain, param=x, qfolds= xval_folds, pagrupa= "clase_ternaria" ),
                          SIMPLIFY= FALSE,
                          mc.cores = 5 )  #debe ir 1 si es Windows
  
  
  ganancia_promedio  <- mean( unlist( vganancias ) )
  #logueo 
  xx  <- x
  xx$xval_folds  <-  xval_folds
  xx$ganancia  <- ganancia_promedio
  xx$iteracion <- GLOBAL_iteracion
  loguear( xx,  arch= archivo_log )
  
  return( xx$ganancia )
}

#------------------------------------------------------------------------------
#Aqui empieza el programa

setwd("~/Desktop/EyF 2022") 

#cargo el dataset, aqui debe poner  SU RUTA
dataset  <- fread("./datasets/competencia1_2022.csv")   #donde entreno

#creo la clase_binaria  SI= {BAJA+1, BAJA+2}  NO={CONTINUA}
dataset[ foto_mes==202101, clase_binaria :=  ifelse( clase_ternaria=="CONTINUA", "NO", "SI" ) ]

#defino los datos donde entreno
dtrain  <- dataset[ foto_mes==202101 ]  #defino donde voy a entrenar
dapply  <- dataset[ foto_mes==202103 ]  #defino donde voy a aplicar el modelo

#-------------------------------------------------
#Transformo en Ranking todas las variables definidas en pesos.

diccionario <- read_ods("./datasets/DiccionarioDatos.ods")
diccionario <- data.table(diccionario)
var.monet <- diccionario[unidad=="pesos", .(campo)]

#rankeo en entrenamiento
dtrain[, paste0("rank_", var.monet$campo) := lapply(.SD, frank),
   .SDcols = var.monet$campo]
dtrain[, var.monet$campo :=NULL] #remuevo las cols monetarias dejo las ranking

#rankeo en entrenamiento
dapply[, paste0("rank_", var.monet$campo) := lapply(.SD, frank),
       .SDcols = var.monet$campo]
dapply[, var.monet$campo :=NULL] #remuevo las cols monetarias dejo las ranking

#-----------------------------------------
#Arma Secreta

#Ramas derechas
dtrain[ , campo1 := as.integer( ctrx_quarter <14 & rank_mcuentas_saldo < -24190 & cprestamos_personales <2 ) ]
dtrain[ , campo2 := as.integer( ctrx_quarter <14 & rank_mcuentas_saldo < -24190 & cprestamos_personales>=2 ) ] 
dtrain[ , campo3 := as.integer( ctrx_quarter <14 & rank_mcuentas_saldo >= -24190 & rank_mcuentas_saldo >= -36385 ) ]
dtrain[ , campo4 := as.integer( ctrx_quarter <14 & rank_mcuentas_saldo >= -24190 & rank_mcuentas_saldo < -36385 ) ]


#Ramas izquierdas
dtrain[ , campo5 := as.integer( ctrx_quarter>=14 & ( Visa_status>=8 | is.na(Visa_status) ) & rank_Visa_mpagominimo >= 80344 ) ]
dtrain[ , campo6 := as.integer( ctrx_quarter>=14 & ( Visa_status>=8 | is.na(Visa_status) ) & rank_Visa_mpagominimo < 80344) ]
dtrain[ , campo7 := as.integer( ctrx_quarter>=14 & Visa_status <8 & !is.na(Visa_status) & ctrx_quarter <38 ) ]
dtrain[ , campo8 := as.integer( ctrx_quarter>=14 & Visa_status <8 & !is.na(Visa_status) & ctrx_quarter>=38 ) ]

#Ramas derechas
dtrain[ , campo1 := as.integer( ctrx_quarter <14 & rank_mcuentas_saldo < -24190 & cprestamos_personales <2 ) ]
dtrain[ , campo2 := as.integer( ctrx_quarter <14 & rank_mcuentas_saldo < -24190 & cprestamos_personales>=2 ) ] 
dtrain[ , campo3 := as.integer( ctrx_quarter <14 & rank_mcuentas_saldo >= -24190 & rank_mcuentas_saldo >= -36385 ) ]
dtrain[ , campo4 := as.integer( ctrx_quarter <14 & rank_mcuentas_saldo >= -24190 & rank_mcuentas_saldo < -36385 ) ]


#Ramas izquierdas
dapply[ , campo5 := as.integer( ctrx_quarter>=14 & ( Visa_status>=8 | is.na(Visa_status) ) & rank_Visa_mpagominimo >= 80344 ) ]
dapply[ , campo6 := as.integer( ctrx_quarter>=14 & ( Visa_status>=8 | is.na(Visa_status) ) & rank_Visa_mpagominimo < 80344) ]
dapply[ , campo7 := as.integer( ctrx_quarter>=14 & Visa_status <8 & !is.na(Visa_status) & ctrx_quarter <38 ) ]
dapply[ , campo8 := as.integer( ctrx_quarter>=14 & Visa_status <8 & !is.na(Visa_status) & ctrx_quarter>=38 ) ]

#-------------------------------------------------


#creo la carpeta donde va el experimento
# HT  representa  Hiperparameter Tuning
dir.create( "./exp/",  showWarnings = FALSE ) 
dir.create( "./exp/HT1109-2/", showWarnings = FALSE )
setwd("./exp/HT1109-2/")   #Establezco el Working Directory DEL EXPERIMENTO

#defino los archivos donde guardo los resultados de la Bayesian Optimization
archivo_log  <- "HT1109-2.txt"
archivo_BO   <- "HT1109-2.RDATA"

#leo si ya existe el log, para retomar en caso que se se corte el programa
GLOBAL_iteracion  <- 0

if( file.exists(archivo_log) )
{
  tabla_log  <- fread( archivo_log )
  GLOBAL_iteracion  <- nrow( tabla_log )
}



#Aqui comienza la configuracion de la Bayesian Optimization

funcion_optimizar  <- EstimarGanancia

configureMlr( show.learner.output= FALSE)

#configuro la busqueda bayesiana,  los hiperparametros que se van a optimizar
#por favor, no desesperarse por lo complejo
obj.fun  <- makeSingleObjectiveFunction(
  fn=       funcion_optimizar,
  minimize= FALSE,   #estoy Maximizando la ganancia
  noisy=    TRUE,
  par.set=  hs,
  has.simple.signature = FALSE   #espia Tomas Delvechio, dejar este parametro asi
)

ctrl  <- makeMBOControl( save.on.disk.at.time= 600,  save.file.path= archivo_BO)
ctrl  <- setMBOControlTermination(ctrl, iters= kBO_iter )
ctrl  <- setMBOControlInfill(ctrl, crit= makeMBOInfillCritEI())

surr.km  <- makeLearner("regr.km", predict.type= "se", covtype= "matern3_2", control= list(trace= TRUE))

#inicio la optimizacion bayesiana
if( !file.exists( archivo_BO ) ) {
  
  run  <- mbo( fun=     obj.fun, 
               learner= surr.km,
               control= ctrl)
  
} else  run  <- mboContinue( archivo_BO )   #retomo en caso que ya exista


#----------------------------------------------------------

# USO TOP N EXPERIMENTOS PARA KAGGLE 
setwd("~/Desktop/EyF 2022") 


experimentos <- fread("./exp/HT1109-2/HT1109-2.txt")

topN <- 5
correr <- experimentos[order(-ganancia), head(.SD, topN)][, .(cp, minsplit, minbucket, maxdepth, iteracion)]


#Función para generar archivos para kaggle. 

fit.predict <- function(param.list, train.set, test.set){
  
  iteracion <- param.list$iteracion
  
  modelo <- rpart(formula =  "clase_binaria ~ . -clase_ternaria",
                  data = train.set,
                  control = param.list[, -"iteracion", with=F])
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
  kaggle.folder <- paste0("KA", dia.mes)
  dir.create( paste0("./exp/", kaggle.folder) )
  
  for( corte  in  c( 8500, 9000, 9500) )
  {
    #le envio a los  corte  mejores,  de mayor probabilidad de prob_SI
    dfinal[ , Predicted := 0L ]
    dfinal[ 1:corte , Predicted := 1L ]
    
    
    fwrite( dfinal[ , list(numero_de_cliente, Predicted) ], #solo los campos para Kaggle
            file= paste0( "./exp/", kaggle.folder,"/",kaggle.folder,"_it_",iteracion,"_corte_", corte, "_binaria_FeatEng.csv"),
            sep=  "," )
  }
  
}


lapply(1:topN, function(x) fit.predict(correr[x], dtrain, dapply))
