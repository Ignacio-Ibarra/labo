require(data.table)
require(ggplot2)
require(gtools)
require(ggfortify)
require(gridExtra)

# require(akmedoids)

setwd("~/Desktop/EyF 2022")

exp633 <- fread("./exp/HT6330/HT6330.txt")

# # Pensaba en hacer kmeans para usar los centroides para armar grupos de ganancia
# get_min_loss <- function(variable, kmax=10){
#   set.seed(123)
#   losses <- unlist(lapply(2:kmax+1, function(x) kmeans(variable, centers = x, nstart=10)$tot.withinss))
#   k <- elbow_point(2:kmax, losses)$x
#   return(round(k,1))
#   }
# 
# get_min_loss(exp633$ganancia, 20)


p1 <- ggplot(exp633, aes(x=iteracion, y=ganancia))+geom_line()
bins <- 5
exp633$gan.bin <- quantcut(exp633$ganancia, q = bins, labels = paste0(c("level"),1:bins))
cols <- c("num.trees","max.depth","min.node.size","mtry")
pca <- prcomp(exp633[, ..cols], scale=T)

png("./work/pcaBORanger.png")
autoplot(pca, data = exp633, colour = "gan.bin",
         loadings = TRUE, 
         loadings.colour = 'black',
         loadings.label = TRUE, 
         loadings.label.size = 3,
         loadings.label.colour = "black")+theme_light()
dev.off()

tab1 <- exp633 %>% group_by(gan.bin) %>% summarise(minimo=min(ganancia), maximo=max(ganancia))

myTable <- tableGrob(
  tab1, 
  rows = NULL, 
  theme = ttheme_default(core = list(bg_params = list(fill = "grey99")))
)
png("./work/bins.png")
grid.draw(myTable)
dev.off()


kable_as_image(tab1)
