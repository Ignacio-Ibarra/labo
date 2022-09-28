rm(list = ls())
gc(verbose = FALSE)

# Librerías necesarias
require("data.table")
require("rpart")
require("ggplot2")
require("lightgbm")
require("xgboost")

# Poner la carpeta de la materia de SU computadora local
# setwd("~/buckets/b1/")
setwd(".")

# Cargamos los datasets y nos quedamos solo con 202101 y 202103
dataset <- fread("./datasets/datasets_muestra25_comp2.csv")   #poner acá el dataset de FE_basico
marzo <- dataset[foto_mes == 202103]
mayo <- dataset[foto_mes == 202105]
rm(dataset)


# Clase BAJA+1 y BAJA+2 juntas
clase_binaria <- ifelse(marzo$clase_ternaria == "CONTINUA", 0, 1)
clase_real <- marzo$clase_ternaria
marzo$clase_ternaria <- NULL
mayo$clase_ternaria <- NULL

x.cols <- names(copy(mayo))

#Creo matriz
dtrain <- xgb.DMatrix(
  data = data.matrix(marzo),
  label = clase_binaria, missing = NA)

set.seed(612337)
params <- list(
  colsample_bynode = 0.8,
  learning_rate = 1,
  max_depth = 3, 
  num_parallel_tree = 10, 
  subsample = 0.8,
  objective = "binary:logistic"
)

xgb_model <- xgb.train(params = params, data = dtrain, nrounds = 1)


new_features <- xgb.create.features(model = xgb_model, data.matrix(marzo))
new_features <- as.data.table(as.data.frame(as.matrix(new_features)))

#Le sumamos canaritos
set.seed(612337)
for (i in 1:20)  {
  new_features[, paste0("canarito", i) := runif(nrow(new_features))]
}


## ---------------------------
## Variable Importance !
## ---------------------------

dtrain_lgb  <- lgb.Dataset(
  data = data.matrix(new_features),
  label = clase_binaria)

mlgb <- lgb.train(
  dtrain_lgb,
  params = list(
    objective = "binary",
    max_bin = 15,
    min_data_in_leaf = 4000,
    learning_rate = 0.05),
  verbose = -1)

var.importance.features <- lgb.importance(mlgb)$Feature
list.canaritos <- grepl("canarito", var.importance.features)
idx <- seq(length(list.canaritos))
tolerancia <- 5
id.canarito.tol <- idx[list.canaritos][tolerancia]
important.features <- var.importance.features[1:id.canarito.tol]
important.new.features <- important.features[grep("V\\d+", important.features)]


final.cols <- append(x.cols, important.new.features)

marzo.final <- xgb.create.features(model = xgb_model, data.matrix(marzo))[, final.cols] 
rm(marzo)
mayo.final  <- xgb.create.features(model = xgb_model, data.matrix(mayo))[, final.cols] 
rm(mayo)

final <- rbind(marzo.final, mayo.final)
final.df <- as.data.table(as.data.frame(as.matrix(final)))
final.df[foto_mes == 202103, clase_ternaria := clase_real]
rm(final)

dia.mes <- format(Sys.Date(),"%d%m")
folder.name <- paste0("./exp/FE", dia.mes,"/")
file.name <- paste0(folder.name, "FE",dia.mes,"_dataset.csv.gz")
dir.create(folder.name, showWarnings = FALSE)

fwrite(final.df, file.name, row.names = F)

