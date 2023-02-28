# Creo variables con xgb create. 
xgbCreateFeatures <- function(params){
  
  pre.rows <- nrow(dataset)
  
  gc()
  cat("XGB Features - Cantidad de Rows Inicio: ", pre.rows)
  train.set <- dataset[ foto_mes < 202106, ]
  temp.set <- dataset[foto_mes == 202106, ]
  test.set <- dataset[ foto_mes == 202107, ]
  
  clase_binaria <- ifelse(train.set$clase_ternaria == "CONTINUA", 0, 1)
  clase_real <- train.set$clase_ternaria
  clase_temp <- temp.set$clase_ternaria
  
  train.set$clase_ternaria <- NULL
  test.set$clase_ternaria <- NULL
  
  x.cols <- names(copy(test.set))
  
  #Creo matriz
  dtrain <- xgb.DMatrix(
    data = data.matrix(train.set),
    label = clase_binaria, missing = NA)
  
  set.seed(612337)
  xgb_model <- xgb.train(params = params, data = dtrain, nrounds = 1)
  
  train.set <- xgb.create.features(model = xgb_model, data.matrix(train.set))
  train.set <- as.data.table(as.data.frame(as.matrix(train.set)))
  
  test.set <- xgb.create.features(model = xgb_model, data.matrix(test.set))
  test.set <- as.data.table(as.data.frame(as.matrix(test.set)))
  
  union <- rbind(train.set, test.set, temp.set)
  post.rows <- nrow(union)
  cat("XGB Features - Cantidad de Rows Final : ", post.rows)
  
  if (pre.rows == post.rows){
    union <- as.data.table(as.data.frame(as.matrix(union)))
    union[foto_mes < 202106, clase_ternaria := clase_real]
    union[foto_mes = 202106, clase_ternaria := clase_temp]
    dataset <- union}
  
  
}
