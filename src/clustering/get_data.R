#este script necesita para correr en Google Cloud
# RAM     16 GB
# vCPU     4
# disco  256 GB


#cluster jerárquico  utilizando "la distancia de Random Forest"
#adios a las fantasias de k-means y las distancias métricas, cuanto tiempo perdido ...
#corre muy lento porque la libreria RandomForest es del Jurasico y no es multithreading

#limpio la memoria
rm( list=ls() )  #remove all objects
gc()             #garbage collection

require("data.table")
require("randomForest")
require("ranger")

#Parametros del script
PARAM <- list()
PARAM$experimento  <- "CLUSTER_UNDER"
# FIN Parametros del script

#------------------------------------------------------------------------------
#------------------------------------------------------------------------------

setwd( "~/buckets/b1/" ) 

#leo el dataset original
# pero podria leer cualquiera que tenga Feature Engineering
dataset  <- fread( "./datasets/competencia3_2022.csv.gz", stringsAsFactors= TRUE)

#creo la carpeta donde va el experimento
dir.create( paste0( "./exp/", PARAM$experimento, "/"), showWarnings = FALSE )
setwd(paste0( "./exp/", PARAM$experimento, "/"))   #Establezco el Working Directory DEL EXPERIMENTO


#me quedo SOLO con los BAJA+2
# dataset  <- dataset[  clase_ternaria =="BAJA+2"  & foto_mes>=202006  & foto_mes<=202105, ]

#undersampleo continua
#grabo los datos donde voy a hacer la optimizacion de hiperparametros

sampling_total <- 0.315
undersampling_continua <- 0.5*nrow(dataset[clase_ternaria %in% c( "BAJA+1", "BAJA+2" )])/nrow(dataset[clase_ternaria=="CONTINUA"])

set.seed( 123456 )
dataset[ foto_mes>=202006  & foto_mes<=202105 , azar_sampling := runif( nrow(dataset[foto_mes>=202006  & foto_mes<=202105]) ) ]

set.seed( 654321 )
dataset[ foto_mes>=202006  & foto_mes<=202105 , azar_under := runif( nrow(dataset[foto_mes>=202006  & foto_mes<=202105]) ) ]

dataset[  , analysis_fold:= 0L ]
dataset[ foto_mes>=202006  & foto_mes<=202105 & 
           ( azar_sampling <= sampling_total ) &
           ( azar_under <= undersampling_continua | clase_ternaria %in% c( "BAJA+1", "BAJA+2" ) )
         , analysis_fold := 1L ]

#quito los nulos para que se pueda ejecutar randomForest,  Dios que algoritmo prehistorico ...
dataset  <- na.roughfix( dataset )


#los campos que arbitrariamente decido considerar para el clustering
#por supuesto, se pueden cambiar
campos_buenos  <- c( "ctrx_quarter", "cpayroll_trx", "mcaja_ahorro", "mtarjeta_visa_consumo", "ctarjeta_visa_transacciones",
                     "mcuentas_saldo", "mrentabilidad_annual", "mprestamos_personales", "mactivos_margen", "mpayroll",
                     "Visa_mpagominimo", "Master_fechaalta", "cliente_edad", "chomebanking_transacciones", "Visa_msaldopesos",
                     "Visa_Fvencimiento", "mrentabilidad", "Visa_msaldototal", "Master_Fvencimiento", "mcuenta_corriente",
                     "Visa_mpagospesos", "Visa_fechaalta", "mcomisiones_mantenimiento", "Visa_mfinanciacion_limite",
                     "mtransferencias_recibidas", "cliente_antiguedad", "Visa_mconsumospesos", "Master_mfinanciacion_limite",
                     "mcaja_ahorro_dolares", "cproductos", "mcomisiones_otras", "thomebanking", "mcuenta_debitos_automaticos",
                     "mcomisiones", "Visa_cconsumos", "ccomisiones_otras", "Master_status", "mtransferencias_emitidas",
                     "mpagomiscuentas")

data_under <- dataset[  analysis_fold==1 ]

fwrite(data_under, 
       "data_under.txt",
       sep= "\t")


