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

#cargo el dataset
dataset  <- fread("./datasets/competencia1_2022.csv")


dataset$foto_mes <- as.factor(dataset$foto_mes)


#genero el modelo,  aqui se construye el arbol
modelo  <- rpart(formula=    "foto_mes ~ . -clase_ternaria",  #quiero predecir clase_ternaria a partir del resto de las variables
                 data=      dataset,  #los datos donde voy a entrenar
                 xval=      0,
                 cp=       -0.3,   #esto significa no limitar la complejidad de los splits
                 minsplit=  2000,     #minima cantidad de registros para que se haga el split
                 minbucket= 1000,     #tamaño minimo de una hoja
                 maxdepth=  4 )    #profundidad maxima del arbol


#grafico el arbol
pdf(file = "./variables_con_drifting.pdf", width=28, height=4)
prp(modelo, extra=101, digits=5, branch=1, type=4, varlen=0, faclen=0)
dev.off()

rpart_splits <- function(fit, digits = getOption("digits")) {
  splits <- fit$splits
  if (!is.null(splits)) {
    ff <- fit$frame
    is.leaf <- ff$var == "<leaf>"
    n <- nrow(splits)
    nn <- ff$ncompete + ff$nsurrogate + !is.leaf
    ix <- cumsum(c(1L, nn))
    ix_prim <- unlist(mapply(ix, ix + c(ff$ncompete, 0), FUN = seq, SIMPLIFY = F))
    type <- rep.int("surrogate", n)
    type[ix_prim[ix_prim <= n]] <- "primary"
    type[ix[ix <= n]] <- "main"
    left <- character(nrow(splits))
    side <- splits[, 2L]
    for (i in seq_along(left)) {
      left[i] <- if (side[i] == -1L)
        paste("<", format(signif(splits[i, 4L], digits)))
      else if (side[i] == 1L)
        paste(">=", format(signif(splits[i, 4L], digits)))
      else {
        catside <- fit$csplit[splits[i, 4L], 1:side[i]]
        paste(c("L", "-", "R")[catside], collapse = "", sep = "")
      }
    }
    cbind(data.frame(var = rownames(splits),
                     type = type,
                     node = rep(as.integer(row.names(ff)), times = nn),
                     ix = rep(seq_len(nrow(ff)), nn),
                     left = left),
          as.data.frame(splits, row.names = F))
  }
}

names(modelo$variable.importance)


unique(dataset$Master_fultimo_cierre)

v <- dataset[Master_fultimo_cierre== 2685,]
