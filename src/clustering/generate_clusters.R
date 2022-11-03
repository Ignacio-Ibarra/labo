require(data.table)
require("randomForest")
require("ranger")
#require(ggfortify)

#Parametros del script
PARAM <- list()
PARAM$experimento  <- "CLUSTER_UNDER"
PARAM$cluster.opt <- "fijo" # puede ser "optimo"
PARAM$nclust <- 7 #seteado por si elige "fijo" puede cambiarse
# FIN Parametros del script

#----------------------------------------------
#Función para evaluar cantidad de clusters optimos. 

eval_clusts <- function(hclust.obj, max.height, data){
  h <- max.height
  results <- list()
  while(  h>1 ){
    cluster.vec  <- cutree( hclust.obj, h)
    mat<-as.matrix(data)
    cat("N Clusters: ", h, "\n")
    #sizeCl <- summary(as.factor(cluster.vec))
    WCSSByClByVar <- tapply(mat, list(rep(cluster.vec,ncol(mat)),col(mat)),
                            function(x) var(x, na.rm=TRUE)*(length(x)-1))
    WCSSByClByVar <- as.data.frame(WCSSByClByVar)
    WCSSByClByVar <- setNames(WCSSByClByVar, names(df))
    WCSSByCl <- rowSums(WCSSByClByVar)
    WCSS <- sum(WCSSByCl)
    res <- list(
      n.clusters = h,
      wss = WCSS 
      #sizeCl=sizeCl
    )
    h <- h - 1 
    results[[length(results)+1]] <- res
  }
  eval <- do.call(rbind.data.frame, results)
  clust.opt <- round(elbow_point(eval$n.clusters, eval$wss)$x)
  return(clust.opt)
}



setwd( "~/buckets/b1/" ) 
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------

#creo la carpeta donde va el experimento
dir.create( paste0( "./exp/", PARAM$experimento, "/"), showWarnings = FALSE )
setwd(paste0( "./exp/", PARAM$experimento, "/"))   #Establezco el Working Directory DEL EXPERIMENTO

data_under <- fread("data_under.txt")

campos_buenos  <- c( "ctrx_quarter", "cpayroll_trx", "mcaja_ahorro", "mtarjeta_visa_consumo", "ctarjeta_visa_transacciones",
                     "mcuentas_saldo", "mrentabilidad_annual", "mprestamos_personales", "mactivos_margen", "mpayroll",
                     "Visa_mpagominimo", "Master_fechaalta", "cliente_edad", "chomebanking_transacciones", "Visa_msaldopesos",
                     "Visa_Fvencimiento", "mrentabilidad", "Visa_msaldototal", "Master_Fvencimiento", "mcuenta_corriente",
                     "Visa_mpagospesos", "Visa_fechaalta", "mcomisiones_mantenimiento", "Visa_mfinanciacion_limite",
                     "mtransferencias_recibidas", "cliente_antiguedad", "Visa_mconsumospesos", "Master_mfinanciacion_limite",
                     "mcaja_ahorro_dolares", "cproductos", "mcomisiones_otras", "thomebanking", "mcuenta_debitos_automaticos",
                     "mcomisiones", "Visa_cconsumos", "ccomisiones_otras", "Master_status", "mtransferencias_emitidas",
                     "mpagomiscuentas")

gc()

modelo  <- randomForest( x= data_under[, campos_buenos, with=FALSE], 
                         y= NULL, 
                         ntree= 1000, #se puede aumentar a 10000
                         proximity= TRUE, 
                         oob.prox=  TRUE )

#genero los clusters jerarquicos
hclust.rf  <- hclust( as.dist ( 1.0 - modelo$proximity),  #distancia = 1.0 - proximidad
                      method= "ward.D2" )



if (PARAM$cluster.opt == "fijo"){
  nclust <- PARAM$nclust
}else{
nclust <- eval_clusts(hclust.rf, 20, data_under[, campos_buenos, with=FALSE])
}
data_under$clusters <- cutree(hclust.rf, nclust)

fwrite(data_under, 
       paste0("data_under_",PARAM$cluster.opt,"_",nclust,"_clusters.txt"),
       sep = "\t")


# pca <- prcomp(data_under[, campos_buenos, with=FALSE], scale = T)
# 
# p1 <- autoplot(pca,
#          data = data_under[, c(campos_buenos, "clase_ternaria"), with=FALSE],
#          colour="clase_ternaria",
#          loadings = TRUE, 
#          loadings.colour = 'black',
#          loadings.label = TRUE, 
#          loadings.label.size = 3,
#          loadings.label.colour = "black")+theme_light()
# 
# 
# var.explicada <- paste(round(100*pca$sdev[1:2]^2 /sum(pca$sdev^2),2), "%", sep="")
# 
# 
# loadings <- as.data.frame(pca$rotation)
# rn <- rownames(loadings)
# pc1<- loadings$PC1
# pc2 <- loadings$PC2
# 
# 
# plot.data <- data.frame(cbind(rn, pc1, pc2))
# plot.data <- melt(plot.data, id.vars = c("rn"), variable.name = "componente", value.name = "loading")
# 
# facet_labels <- c(
#   pc1 = paste0("PC1: ", var.explicada[1]),
#   pc2 = paste0("PC2: ", var.explicada[2])
# )
# 
# p2 <- ggplot(plot.data) +
#   aes(x = rn, weight = as.numeric(loading)) +
#   geom_bar(position = "dodge", fill = "#4682B4") +
#   labs(x = "Variables", y = "Carga") +
#   coord_flip() +
#   theme_light() +
#   theme(
#     axis.title.y = element_text(size = 12L),
#     axis.title.x = element_text(size = 12L)
#   ) +
#   facet_wrap(vars(componente), ncol = 2L, nrow = 2L, labeller = as_labeller(facet_labels))
# 
# under.pca$rotation
