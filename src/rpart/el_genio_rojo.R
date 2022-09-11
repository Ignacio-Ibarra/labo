#Limpieza de memoria
rm( list=ls() )  
gc()   

#cargo las librerias que necesito
require("data.table")
require("rpart")
require("rpart.plot")
require(readODS)

#Establezco el Working Directory
setwd("~/Desktop/EyF 2022")  

#cargo el dataset
dataset  <- fread("./datasets/competencia1_2022.csv")

#-------------------------------------------------------------------
# Clases binarias y separo train de test

#creo la clase_binaria SI={ BAJA+1, BAJA+2 }    NO={ CONTINUA }
dataset[ foto_mes==202101, 
         clase_binaria :=  ifelse( clase_ternaria=="CONTINUA", "NO", "SI" ) ]

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

#rankeo en test
dapply[, paste0("rank_", var.monet$campo) := lapply(.SD, frank), 
       .SDcols = var.monet$campo]
dapply[, var.monet$campo :=NULL] #remuevo las cols monetarias dejo las ranking


#Corrijo las variables con mayor drifting y que tienen relavancia mayor al canarito. 

# Visa_fultimo_cierre
dapply[ Visa_fultimo_cierre== 1, Visa_fultimo_cierre :=  4 ]
dapply[ Visa_fultimo_cierre== 7, Visa_fultimo_cierre := 11 ]
dapply[ Visa_fultimo_cierre==21, Visa_fultimo_cierre := 25 ]
dapply[ Visa_fultimo_cierre==14, Visa_fultimo_cierre := 18 ]
dapply[ Visa_fultimo_cierre==28, Visa_fultimo_cierre := 32 ]
dapply[ Visa_fultimo_cierre==35, Visa_fultimo_cierre := 39 ]
dapply[ Visa_fultimo_cierre> 39, Visa_fultimo_cierre := Visa_fultimo_cierre + 4 ]

# Master_fultimo_cierre
dapply[ Master_fultimo_cierre== 1, Master_fultimo_cierre :=  4 ]
dapply[ Master_fultimo_cierre== 7, Master_fultimo_cierre := 11 ]
dapply[ Master_fultimo_cierre==21, Master_fultimo_cierre := 25 ]
dapply[ Master_fultimo_cierre==14, Master_fultimo_cierre := 18 ]
dapply[ Master_fultimo_cierre==28, Master_fultimo_cierre := 32 ]
dapply[ Master_fultimo_cierre==35, Master_fultimo_cierre := 39 ]
dapply[ Master_fultimo_cierre> 39, Master_fultimo_cierre := Master_fultimo_cierre + 4 ]

# Visa_fechaalta
visa.corregir <- floor(mean(dapply$Visa_fechaalta, na.rm = T) - mean(dtrain$Visa_fechaalta, na.rm = T))
dapply[, Visa_fechaalta := Visa_fechaalta - visa.corregir] #Corro la cantidad de dias

# Master_fechaalta
master.corregir <- floor(mean(dapply$Master_fechaalta , na.rm = T) - mean(dtrain$Master_fechaalta, na.rm = T))
dapply[, Master_fechaalta := Master_fechaalta - master.corregir] #Corro la cantidad de dias

#-------------------------------------------------




# Entreno el modelo
# obviamente rpart no puede ve  clase_ternaria para predecir  clase_binaria
#  #no utilizo Visa_mpagado ni  mcomisiones_mantenimiento por drifting

modelo  <- rpart(formula=   "clase_binaria ~ . -clase_ternaria",
                 data=      dtrain,  #los datos donde voy a entrenar
                 xval=        0,
                 cp=        -0.54,#  -0.89
                 minsplit=  1073,   # 621
                 minbucket=  278,   # 309
                 maxdepth=     9 )  #  12

