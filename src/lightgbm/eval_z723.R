rm( list=ls() )  #remove all objects
gc()             #garbage collection

require(data.table)
require(ggplot2)
require(gtools)
require(ggfortify)
require(gridExtra)
require(dplyr)
require(grid)
require(kableExtra)


# require(akmedoids)

setwd("~/Desktop/EyF 2022")

exp7231 <- fread("./exp/HT7231/HT7231.txt")

# # Pensaba en hacer kmeans para usar los centroides para armar grupos de ganancia
# get_min_loss <- function(variable, kmax=10){
#   set.seed(123)
#   losses <- unlist(lapply(2:kmax+1, function(x) kmeans(variable, centers = x, nstart=10)$tot.withinss))
#   k <- elbow_point(2:kmax, losses)$x
#   return(round(k,1))
#   }
# 
# get_min_loss(exp7231$ganancia, 20)


p1 <- ggplot(exp7231, aes(x=iteracion, y=ganancia))+geom_line()+ylim(2.65e+07, 2.8e+07)

cols <- c("num_iterations","learning_rate","feature_fraction","min_data_in_leaf","num_leaves","envios","ganancia","iteracion")
eval.params <- c("num_iterations","learning_rate","feature_fraction","min_data_in_leaf","num_leaves","envios")

#descarto lo que no me sirve
eval.df <- exp7231[ganancia>= 2.65e+07, ..cols]

#evaluo
bins <- 4
eval.df$gan.bin <- quantcut(eval.df$ganancia, q = bins, labels = paste0(c("level"),1:bins))

pca <- prcomp(eval.df[, ..eval.params], scale=T)

png("./work/pcaBO_LGB.png")
autoplot(pca, data = eval.df, colour = "gan.bin",
         loadings = TRUE, 
         loadings.colour = 'black',
         loadings.label = TRUE, 
         loadings.label.size = 3,
         loadings.label.colour = "black")+theme_light()
dev.off()

tab1 <- eval.df %>% group_by(gan.bin) %>% summarise(minimo=min(ganancia), maximo=max(ganancia))

myTable <- tableGrob(
  tab1, 
  rows = NULL, 
  theme = ttheme_default(core = list(bg_params = list(fill = "grey99")))
)
png("./work/bins.png")
grid.draw(myTable)
dev.off()

as_image(tab1)
