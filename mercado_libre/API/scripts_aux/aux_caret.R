## Script donde se realiza el proceso de hiper parameter tunning para 
# RF , BOOSTING y SVR

# Librerias

library(tidyverse)
library(here)
library(caret)
library(doParallel)

options(scipen = 999)

# Funciones auxiliares

source(here("mercado_libre/API/funciones","funcion_imput_media.R"))

### DATOS

aptos_yearmonth <- list.files(path = here("mercado_libre/API/datos/limpios/apt"), 
                              pattern = "*.csv", full.names = T)

yearmonth <- c('aptos_202106','aptos_202107',"aptos_202108","aptos_202109")


aptos <- sapply(aptos_yearmonth, FUN=function(yearmonth){
      read_csv(file=yearmonth)}, simplify=FALSE) %>% bind_rows()


aptos <- aptos %>% group_by(id) %>% 
      arrange(desc(fecha_bajada)) %>%
      slice(1) %>% ungroup()

aptos <- aptos %>% mutate_if(is.character, as.factor)


# Filtramos por el criterio en price:

aptos_todos <- aptos

aptos <- aptos %>% filter(price <= quantile(aptos$price,.95))

# Perdemos esta cantidad de registros

nrow(aptos_todos) - nrow(aptos)


# vemos prop de NA
p_na <- sapply(aptos, function(x) round(sum(is.na(x))/length(x),4)) %>% data.frame() %>% 
      rename(prop_na=".") %>% arrange(desc(prop_na))


#### Definicion de objetos para realizar el procedimiento 

## 2 casos segun el metodo de imputacion

## 1) Imputacion por media

#### Definimos variables Sin na

aptos_sin_na <- imput_media(aptos,p=.1)

# Variable de entrada y variables de salida

aptos_y <- aptos_sin_na$price

aptos_x <- aptos_sin_na %>% select(-price)

# Creamos Folds para hacer cross-validation

set.seed(1234)

myFolds <- createFolds(aptos_y, k = 5) # Usamos 5-folds por tiempo computacional

# Create reusable trainControl object: myControl
myControl <- trainControl(
   method='cv',
   verboseIter = TRUE,
   savePredictions = TRUE,
   index = myFolds
)

## 2) Imputacion por MissRanger

# Cargamos los datos

aptos_mr <- read_csv(here("mercado_libre/API/datos/limpios/apt","aptos_mr.csv"))

aptos_mr <- aptos_mr %>% mutate_if(is.character, as.factor)

# Variable de entrada y variables de salida

aptos_y_mr <- aptos_mr$price

aptos_x_mr <- aptos_mr %>% select(-price)

# Creamos Folds para hacer cross-validation

set.seed(1234)

myFolds_mr <- createFolds(aptos_y_mr, k = 5) # Usamos 5 folds por tiempo computacional

# Create reusable trainControl object: myControl
myControl_mr <- trainControl(
   method='cv',
   verboseIter = TRUE,
   savePredictions = TRUE,
   index = myFolds_mr
)

##################################################################
############################## RF ################################
##################################################################

#################################################################
# Caso 1: imputamos solo variables cuantitativas con % NA < 0.1 #
#################################################################

# 1er - Modelo Random Forest

no_cores <- detectCores(logical = FALSE)

cl <- makePSOCKcluster(no_cores)

registerDoParallel(cl)

clusterSetRNGStream(cl, iseed=12345)

pracma::tic()


RF_caret <- train(
      x = aptos_x, 
      y = aptos_y,
      method = 'ranger',
      trControl = myControl
)


## When you are done:
stopCluster(cl)

registerDoSEQ() # Para volver a "modo secuencial"

pracma::toc()

# Resultados del modelo

RF_caret

# Vemos graficamente el comportamiento de los hiperparametros
plot(RF_caret)

# Definimos grilla segun lo que vemos

tuneGrid <- data.frame(
   .mtry = c(12:20),
   .splitrule = "variance", #rule to split on
   .min.node.size = 5
)


# Hacemos ajuste de los hiperparametros con parallel

# Proceso de paralelo

# Calculate the number of cores
no_cores <- detectCores(logical = FALSE)

cl <- makePSOCKcluster(no_cores)

registerDoParallel(cl)

clusterSetRNGStream(cl, iseed=12345) # Para lograr reproducibilidad

pracma::tic()

RF_caret_tunning <- train(
   x = aptos_x, 
   y = aptos_y,
   tuneGrid = tuneGrid,
   method = 'ranger',
   trControl = myControl,
   preProcess = c("nzv")
)

## When you are done:
stopCluster(cl)

registerDoSEQ() # Para volver a "modo secuencial"

pracma::toc()

########################################
# Caso 2: imputamos usando Miss Ranger #
########################################

# 1er - Modelo Random Forest

no_cores <- detectCores(logical = FALSE)

cl <- makePSOCKcluster(no_cores)

registerDoParallel(cl)

clusterSetRNGStream(cl, iseed=12345)

pracma::tic()

RF_caret_mr <- train(
   x = aptos_x_mr, 
   y = aptos_y_mr,
   method = 'ranger',
   trControl = myControl_mr
)


## When you are done:
stopCluster(cl)

registerDoSEQ() # Para volver a "modo secuencial"

pracma::toc()

# Resultados del modelo

RF_caret_mr

# Vemos graficamente el comportamiento de los hiperparametros
plot(RF_caret_mr)

# Definimos grilla segun lo que vemos

tuneGrid <- data.frame(
   .mtry = c(12:20),
   .splitrule = "variance", #rule to split on
   .min.node.size = 5
)


# Hacemos ajuste de los hiperparametros con parallel

# Proceso de paralelo

# Calculate the number of cores
no_cores <- detectCores(logical = FALSE)

cl <- makePSOCKcluster(no_cores)

registerDoParallel(cl)

clusterSetRNGStream(cl, iseed=12345) # Para lograr reproducibilidad

pracma::tic()

RF_caret_tunning_mr <- train(
   x = aptos_x_mr, 
   y = aptos_y_mr,
   tuneGrid = tuneGrid,
   method = 'ranger',
   trControl = myControl_mr,
   preProcess = c("nzv")
)

## When you are done:
stopCluster(cl)

registerDoSEQ() # Para volver a "modo secuencial"

pracma::toc()



#########################################################################
############################ Boosting ###################################
#########################################################################

# Cargamos funcion para hacer CV en boosting

source(here("mercado_libre/API/funciones","funcion_tunning_boosting.R"))


#################################################################
# Caso 1: imputamos solo variables cuantitativas con % NA < 0.1 #
#################################################################

## Ajustamos el modelo a los valores por defecto de caret para tener un 
# punto de comparacion

b_grilla_inicial  <- expand.grid(
   interaction.depth = c(1,2,3),
   n.trees = c(50,100,150), #rule to split on
   shrinkage = c(0.1),
   n.minobsinnode = 10
)


# 1er - Boosting

no_cores <- detectCores(logical = FALSE)

cl <- makePSOCKcluster(no_cores)

registerDoParallel(cl)

clusterSetRNGStream(cl, iseed=12345)

pracma::tic()


Boosting_caret <- tunning_boosting(datos=aptos_sin_na,
                                   grilla = b_grilla_inicial,
                                   k=5,
                                   seed = 12345,
                                   parallel = T,
                                   control = myFolds)
   
## When you are done:
stopCluster(cl)

registerDoSEQ() # Para volver a "modo secuencial"

pracma::toc()

# Resultados del modelo

Boosting_caret$tunning

# Vemos graficamente el comportamiento de los hiperparametros

Boosting_caret$tunning %>% mutate(max_tree_depth = factor(interaction.depth)) %>% 
   ggplot(aes(y=RMSE, x= n.trees,color=max_tree_depth)) + 
   geom_point()+ geom_line()


# Busqueda orientada en el espacio de los hiperparametros
# (adaptive resampling)

# Definimos grilla segun lo que vemos

# Define the tuning grid: tuneGrid

tuneGrid <- expand.grid(
   interaction.depth = c(3,4),
   n.trees = c(500,1500,2000,5000), 
   shrinkage = c(0.1),
   n.minobsinnode = 10
)


# Hacemos ajuste de los hiperparametros con parallel

# Proceso de paralelo

# Calculate the number of cores
no_cores <- detectCores(logical = FALSE)

cl <- makePSOCKcluster(no_cores)

registerDoParallel(cl)

clusterSetRNGStream(cl, iseed=12345) # Para lograr reproducibilidad

pracma::tic()

Boosting_caret_tunning <- tunning_boosting(datos=aptos_sin_na,
                                   grilla = tuneGrid,
                                   k=5,
                                   seed = 12345,
                                   parallel = T,
                                   control = myFolds)

## When you are done:
stopCluster(cl)

registerDoSEQ() # Para volver a "modo secuencial"

pracma::toc()

## Resultados

Boosting_caret_tunning$tunning


########################################
# Caso 2: imputamos usando Miss Ranger #
########################################

## Ajustamos el modelo a los valores por defecto de caret para tener un 
# punto de comparacion

b_grilla_inicial  <- expand.grid(
   interaction.depth = c(1,2,3),
   n.trees = c(50,100,150), #rule to split on
   shrinkage = c(0.1),
   n.minobsinnode = 10
)


# 1er - Boosting

no_cores <- detectCores(logical = FALSE)

cl <- makePSOCKcluster(no_cores)

registerDoParallel(cl)

clusterSetRNGStream(cl, iseed=12345)

pracma::tic()


Boosting_caret_mr <- tunning_boosting(datos=aptos_mr,
                                   grilla = b_grilla_inicial,
                                   k=5,
                                   seed = 12345,
                                   parallel = T,
                                   control = myFolds_mr)

## When you are done:
stopCluster(cl)

registerDoSEQ() # Para volver a "modo secuencial"

pracma::toc()

# Resultados del modelo

Boosting_caret_mr$tunning

# Vemos graficamente el comportamiento de los hiperparametros

Boosting_caret_mr$tunning %>% mutate(max_tree_depth = factor(interaction.depth)) %>% 
   ggplot(aes(y=RMSE, x= n.trees,color=max_tree_depth)) + 
   geom_point()+ geom_line()


# Definimos grilla segun lo que vemos

# Define the tuning grid: tuneGrid

tuneGrid <- expand.grid(
   interaction.depth = c(3,4),
   n.trees = c(500,1500,2000,5000), 
   shrinkage = c(0.1),
   n.minobsinnode = 10
)


# Hacemos ajuste de los hiperparametros con parallel

# Proceso de paralelo

# Calculate the number of cores
no_cores <- detectCores(logical = FALSE)

cl <- makePSOCKcluster(no_cores)

registerDoParallel(cl)

clusterSetRNGStream(cl, iseed=12345) # Para lograr reproducibilidad

pracma::tic()

Boosting_caret_tunning_mr <- tunning_boosting(datos=aptos_mr,
                                           grilla = tuneGrid,
                                           k=5,
                                           seed = 12345,
                                           parallel = T,
                                           control = myFolds_mr)

## When you are done:
stopCluster(cl)

registerDoSEQ() # Para volver a "modo secuencial"

pracma::toc()

## Resultados

Boosting_caret_tunning_mr$tunning



#########################################################################
############################ SVR ###################################

#################################################################
# Caso 1: imputamos solo variables cuantitativas con % NA < 0.1 #
#################################################################

#### Definimos variables Sin na

aptos_sin_na <- imput_media(aptos,p=.1)

# Create custom indices: myFolds

aptos_y <- aptos_sin_na$price

set.seed(12345)

myFolds <- createFolds(aptos_y, k = 5)

# Create reusable trainControl object: myControl
myControl <- trainControl(
   verboseIter = TRUE,
   savePredictions = TRUE,
   index = myFolds
)

# Definimos la martiz x de variables regresoras
aptos_x <- aptos_sin_na %>% select(-price)


# Proceso de paralelo

# Calculate the number of cores
no_cores <- detectCores(logical = FALSE)

cl <- makePSOCKcluster(no_cores)

registerDoParallel(cl)

clusterSetRNGStream(cl, iseed=12345) # Para lograr reproducibilidad

pracma::tic()

SVR_caret<- train(
   price ~ .,
   data = aptos_sin_na,
   method = 'svmRadial',
   trControl = myControl,
   preProcess = c("nzv")
)


## When you are done:
stopCluster(cl)

registerDoSEQ() # Para volver a "modo secuencial"

pracma::toc()


plot(SVR_caret)

######### Hyperparameter tuning


tuneGrid <- expand.grid(
   .sigma = c(.03085),
   .C = c(1,2,3)
)



# Calculate the number of cores
no_cores <- detectCores(logical = FALSE)

cl <- makePSOCKcluster(no_cores)

registerDoParallel(cl)

clusterSetRNGStream(cl, iseed=12345) # Para lograr reproducibilidad

pracma::tic()

SVR_caret_tunning<- train(
   price ~ .,
   data = aptos_sin_na,
   tuneGrid = tuneGrid,
   method = 'svmRadial',
   trControl = myControl,
   preProcess = c("nzv")
)



## When you are done:
stopCluster(cl)

registerDoSEQ() # Para volver a "modo secuencial"

pracma::toc()



#################

getModelInfo("svmRadial")$svmRadial$parameters


SVM_train_simulado_1 <- train(
   Y ~ ., 
   data = simulado_1,
   method = "svmRadial",               
   trControl = trainControl(method = "cv", number = 10),
   tuneLength = 30
)

