#Arbol elemental con libreria  rpart
#Debe tener instaladas las librerias  data.table  ,  rpart  y  rpart.plot
rm( list=ls() )  
gc()   

#cargo las librerias que necesito
require("data.table")
require("rpart")
require("rpart.plot")

#Aqui se debe poner la carpeta de la materia de SU computadora local
setwd("~/Desktop/EyF 2022")  #Establezco el Working Directory

#cargo el dataset donde voy a entrenar
dataset  <- fread("./datasets/datasets_muestra252_comp2.csv")

dataset  <- dataset[  foto_mes %in% c( 202103, 202105 ) ]


#genero el modelo,  aqui se construye el arbol
modelo.drifting  <- rpart(formula=    "foto_mes ~ . -clase_ternaria",  #quiero predecir clase_ternaria a partir del resto de las variables
                 data=      dataset,  #los datos donde voy a entrenar
                 xval=      0,
                 cp=       -0.3,   #esto significa no limitar la complejidad de los splits
                 minsplit=  2000,     #minima cantidad de registros para que se haga el split
                 minbucket= 1000,     #tamaño minimo de una hoja
                 maxdepth=  4 )    #profundidad maxima del arbol


campos.drifting <- names(modelo.drifting$variable.importance)
campos.drifting <- setdiff(campos.drifting,c("numero_de_cliente"))


#------------------------------------------------------------------------------

graficar_campo  <- function( campo, campo_clase, valores_clase )
{

  #quito de grafico las colas del 5% de las densidades
  qA  <- quantile(  dataset[ foto_mes==202103 , get(campo) ] , prob= c(0.05, 0.95), na.rm=TRUE )
  qB  <- quantile(  dataset[ foto_mes==202105 , get(campo) ] , prob= c(0.05, 0.95), na.rm=TRUE )

  xxmin  <- pmin( qA[[1]], qB[[1]] )
  xxmax  <- pmax( qA[[2]], qB[[2]] )

  densidad_A  <- density( dataset[ foto_mes==202103 & get(campo_clase) %in% valores_clase, get(campo) ],
                          kernel="gaussian", na.rm=TRUE )

  densidad_B  <- density( dataset[ foto_mes==202105 & get(campo_clase) %in% valores_clase, get(campo) ],
                          kernel="gaussian", na.rm=TRUE )

  plot( densidad_A,
        col="blue",
        xlim= c( xxmin, xxmax ),
        ylim= c( 0, pmax( max(densidad_A$y), max(densidad_B$y) ) ),
        main= paste0( campo, ",   ", campo_clase, " in ",  paste( valores_clase,collapse=",")) 
      )

  lines(densidad_B, col="red", lty=2)
  
  legend(  "topright",  
           legend=c("202003", "202005"),
           col=c("blue", "red"), lty=c(1,2))

}
#------------------------------------------------------------------------------

dataset[ foto_mes==202103, 
         clase_binaria :=  ifelse( clase_ternaria=="CONTINUA", "NO", "SI" ) ]

# Entreno el modelo
# utilizo los mejores hiperparametros encontrados en una Bayesian Optimizationcon 5-fold Cross Validation

modelo  <- rpart(formula=   "clase_binaria ~ . -clase_ternaria",
                 data=      dataset[ foto_mes==202103 ],  #los datos donde voy a entrenar
                 xval=         0,
                 cp=           -0.69,
                 minsplit=    870,
                 minbucket=     9,
                 maxdepth=      9)


campos.modelo  <- names( modelo$variable.importance )
campos.malos <- campos.drifting[campos.drifting %in%  campos.modelo]



graficar_campo  <- function( campo, campo_clase, valores_clase )
{
  
  #quito de grafico las colas del 5% de las densidades
  qA  <- quantile(  dataset[ foto_mes==202103 , get(campo) ] , prob= c(0.05, 0.95), na.rm=TRUE )
  qB  <- quantile(  dataset[ foto_mes==202105 , get(campo) ] , prob= c(0.05, 0.95), na.rm=TRUE )
  
  xxmin  <- pmin( qA[[1]], qB[[1]] )
  xxmax  <- pmax( qA[[2]], qB[[2]] )
  
  densidad_A  <- density( dataset[ foto_mes==202103 & get(campo_clase) %in% valores_clase, get(campo) ],
                          kernel="gaussian", na.rm=TRUE )
  
  densidad_B  <- density( dataset[ foto_mes==202105 & get(campo_clase) %in% valores_clase, get(campo) ],
                          kernel="gaussian", na.rm=TRUE )
  
  plot( densidad_A,
        col="blue",
        xlim= c( xxmin, xxmax ),
        ylim= c( 0, pmax( max(densidad_A$y), max(densidad_B$y) ) ),
        main= paste0( campo, ",   ", campo_clase, " in ",  paste( valores_clase,collapse=",")) 
  )
  
  lines(densidad_B, col="red", lty=2)
  
  legend(  "topright",  
           legend=c("202001", "202003"),
           col=c("blue", "red"), lty=c(1,2))
  
}

dia.mes <- format(Sys.Date(),"%d%m")

# dir.create( "./exp/",  showWarnings = FALSE ) 
dir.create( paste0("./exp/DR",dia.mes,"/"), showWarnings = FALSE )
setwd("./exp/DR6130/")

pdf("densidades_drifting_campos_malos2.pdf")

for( campo in  campos.malos )
{
  cat( campo, "  " )
  
  #graficar_campo( campo, "clase_ternaria", c( "BAJA+1", "BAJA+2", "CONTINUA" ) )
  graficar_campo( campo, "clase_ternaria", c( "BAJA+1", "BAJA+2" ) )
  # graficar_campo( campo, "clase_ternaria", c( "BAJA+2" ) )
  # graficar_campo( campo, "clase_ternaria", c( "BAJA+1" ) )
  # graficar_campo( campo, "clase_ternaria", c( "CONTINUA" ) )
}

dev.off()

dataset[ foto_mes==202105 & get("clase_ternaria") %in% c("BAJA+1","BAJA+2"),]

# 1) Visa_fechaalta
diff.Visa_fechaalta <- round(mean(dataset[foto_mes==202103, Visa_fechaalta], na.rm = T)) - round(mean(dataset[foto_mes==202101, Visa_fechaalta], na.rm = T))
dataset[foto_mes==202103, Visa_fechaalta:= Visa_fechaalta - diff.Visa_fechaalta]

# 2) 
master_mpagado01 <- frank(dataset[foto_mes==202101, Master_mpagado], ties.method = "random", na.last = "keep")
master_mpagado03 <- frank(dataset[foto_mes==202103, Master_mpagado], ties.method = "random", na.last = "keep")



