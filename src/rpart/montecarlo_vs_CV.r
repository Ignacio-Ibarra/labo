rm( list=ls() )  
gc()   

require("data.table")
require("rpart")
require("parallel")
require("caret")
require(ggplot2)
require(ggridges)


setwd("~/Desktop/EyF 2022")  



dataset  <- fread("./datasets/competencia1_2022.csv")

denero <- dataset[ foto_mes==202101 ]

#------------------------------------------------------------------------

EntrenarParticion <- function(x, particion){
  ids <- particion[[x]] 
  train.set <- denero[ids]
  val.set <- denero[-ids]
  #genero el modelo
  modelo  <- rpart("clase_ternaria ~ .",     
                   data= train.set, 
                   xval= 0,
                   control=list( "cp"=    -1,  
                                 "minsplit"=  300,  
                                 "minbucket"= 150,  
                                 "maxdepth"=    6 )
                   )  
  
  
  prediccion  <- predict( modelo,   
                          val.set,  
                          type= "prob") 
  
  
  ganancia_test  <- val.set[, 
                            sum( ifelse( prediccion[, "BAJA+2"]  >  0.025,
                                         ifelse( clase_ternaria=="BAJA+2", 78000, -2000 ),
                                         0 ) )]

  ganancia_test_normalizada  <-  ganancia_test / 0.3
  
  return(ganancia_test_normalizada)
}




#------------------------------------------------------------------------

ArbolEstimarGananciaMontecarlo  <- function(sema)
{
  set.seed(sema)
  sprintf("\nExperimento Montecarlo con semilla: %d", sema)
  particion <- createDataPartition(denero$clase_ternaria, p= 0.8, list=TRUE, times=3)
  resultado.exp <- unlist(lapply(1:3, function(x) EntrenarParticion(x, particion)))
  return(list("semilla" = sema,
              "tipo" = "MonteCarlo",
              "media" = mean(resultado.exp), 
              "desvio"=sd(resultado.exp)))
}

#------------------------------------------------------------------------------

ArbolEstimarGananciaCV  <- function(sema)
{ 
  set.seed(sema)
  sprintf("\nExperimento Cross-Validation con semilla: %d", sema)
  particion <- createFolds(denero$clase_ternaria, k=5, returnTrain = T)
  particion.list <- TRUE
  resultado.exp <- unlist(lapply(1:3, function(x) EntrenarParticion(x, particion)))
  return(list("semilla" = sema,
              "tipo" = "CV",
              "media" = mean(resultado.exp), 
              "desvio"=sd(resultado.exp)))
}



#------------------------------------------------------------------------------



ksemillas  <- 1:100




salidasMC <- mcmapply( ArbolEstimarGananciaMontecarlo, 
                        ksemillas,   
                        SIMPLIFY= FALSE,
                        mc.cores= 2) 


salidasCV <- mcmapply( ArbolEstimarGananciaCV, 
                       ksemillas,   
                       SIMPLIFY= FALSE,
                       mc.cores= 2) 

#paso la lista a df
mc_salida  <- data.frame(rbindlist(salidasMC))

cv_salida <- data.frame(rbindlist(salidasCV))


write.csv(mc_salida, "./TareasHogar/mc_salida100.csv", row.names=F)
write.csv(cv_salida, "./TareasHogar/cv_salida100.csv", row.names=F)

#-------------------------------------------------

mc_salida <- fread("./TareasHogar/mc_salida100.csv")
cv_salida <- fread("./TareasHogar/cv_salida100.csv")

salidas <- rbind(mc_salida, cv_salida)

png("./TareasHogar/dist_desvios.png")
ggplot(salidas, aes(x = tipo, y = desvio, fill=tipo)) + 
  ggdist::stat_halfeye(
    adjust = .5, 
    width = .6, 
    .width = 0, 
    justification = -.2, 
    point_colour = NA
  ) + 
  geom_boxplot(
    width = .15, 
    outlier.shape = 19
  ) +
  ## add justified jitter from the {gghalves} package
  gghalves::geom_half_point(
    ## draw jitter on the left
    side = "l", 
    ## control range of jitter
    range_scale = .4, 
    ## add some transparency
    alpha = .3
  ) +
  theme_ridges()+
  coord_cartesian(xlim = c(1.2, NA), clip = "off")+
  ylab("")+ggtitle("CV vs MonteCarlo")+
  theme(plot.title = element_text(size=12, hjust=0.5, face="plain"),
        legend.position="none",
        axis.text.x = element_text(size = 10, face = "plain"),
        axis.title.x = element_text(size = 10, face = "plain"),
        axis.text.y = element_text(size = 10, face = "plain"),
        axis.title.y = element_text(size = 10, face = "plain"))
dev.off()





require(dplyr)
salidas %>% group_by(tipo) %>% summarise(desv = sd(desvio))
