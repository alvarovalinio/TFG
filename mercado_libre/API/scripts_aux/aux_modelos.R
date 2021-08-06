################
#### MODELS ####
################

rm(list=ls())

library(tidyverse)
library(here)
library(caret)
library(doParallel)
#library(pracma)
library(rpart)
library(rpart.plot)
library(rattle)

#### DATOS

aptos_yearmonth <- list.files(path = here("mercado_libre/API/datos/limpios/apt"), 
                              pattern = "*.csv", full.names = T)

yearmonth <- c('aptos_202106','aptos_202107')


aptos <- sapply(aptos_yearmonth, FUN=function(yearmonth){
  read_csv(file=yearmonth)}, simplify=FALSE) %>% bind_rows()


aptos <- aptos %>% group_by(id) %>% 
  arrange(desc(fecha_bajada)) %>%
  slice(1) %>% ungroup()

aptos <- aptos %>% mutate_if(is.character, as.factor)

# vemos prop de NA
p_na <- sapply(aptos, function(x) round(sum(is.na(x))/length(x),4)) %>% data.frame() %>% 
  rename(prop_na=".") %>% arrange(desc(prop_na))

# Guardamos variables que tiene NA

v_NA <- p_na %>% filter(prop_na>0) %>% row.names()

##### Arbol de regresion

set.seed(1234)

aptos_sin_na <- aptos %>% select(-c(all_of(v_NA),id,title,accepts_mercadopago,city_name,
                                 latitude,longitude,date_created,last_updated,
                                 covered_area_unidad,property_type,tipo_cambio,
                                 year_month,lat_barrio,fecha_bajada,condition))

arbol <- rpart(price~ . , data=aptos_sin_na)


summary(arbol)

# Proceso de poda

broom::tidy(arbol$cptable)

# Grafico de la evolucion del error

cp_error <- data.frame(arbol$cptable)

cp_error %>% ggplot(aes(x=CP,y=xerror))+geom_point(color="red")+geom_line()

# Otra forma

plotcp(arbol) 

# Obtengamos el cp

cp_opt <- arbol$cptable[which.min(arbol$cptable[,"xerror"]),"CP"]

# Arbol podado:

arbol.prune <- rpart(price ~ . , data = aptos_sin_na,method="anova",
                     control=rpart.control(cp=cp_opt))


# Grafico

rpart.plot(arbol.prune,roundint = T,digits = 4)

# TRAIN CONTROL CARET

# Create custom indices: myFolds
aptos_y <- aptos$price

set.seed(12345)

myFolds <- createFolds(aptos_y, k = 10)

# Create reusable trainControl object: myControl
myControl <- trainControl(
      verboseIter = TRUE,
      savePredictions = TRUE,
      index = myFolds
)

# Arbol con Caret

factores <- aptos %>% select_if(~is.factor(.)) 
factores <- factores[,colnames(factores)%in%v_NA] # Sacamos factores que tienen NA

# Despues imputamos alguno
# eliminamos variables numericas con proporcion de NA < 0.1
 
aptos_sin_na <- aptos %>% select(-c(colnames(factores),id,title,accepts_mercadopago,city_name,
                                    latitude,longitude,date_created,last_updated,
                                    covered_area_unidad,property_type,tipo_cambio,
                                    year_month,lat_barrio,fecha_bajada,condition,
                                    unit_floor, floors, rooms,maintenance_fee))

aptos_x <- aptos_sin_na %>% select(-price)

#set.seed(12345)

# Proceso de paralelo

# Calculate the number of cores
no_cores <- detectCores(logical = FALSE)

cl <- makePSOCKcluster(no_cores)

registerDoParallel(cl)

clusterSetRNGStream(cl, iseed=12345) # Para lograr reproducibilidad

pracma::tic()

arbol_caret <- train(
      x = aptos_x, 
      y = aptos_y,
      method = 'rpart',
      trControl = myControl,
      preProcess = "medianImpute"
)

## When you are done:
stopCluster(cl)

registerDoSEQ() # Para volver a "modo secuencial"

pracma::toc()

fancyRpartPlot(median_model$finalModel)


#### CARET

# Modelo de regresión multiple

pracma::tic()

no_cores <- detectCores(logical = FALSE)

cl <- makePSOCKcluster(no_cores)

registerDoParallel(cl)

clusterSetRNGStream(cl, iseed=12345)

clusterEvalQ(cl,'myControl')

lm_caret <- train(
      x = aptos_x, 
      y = aptos_y,
      method = 'lm',
      trControl = myControl,
      preProcess = c("medianImpute", "nzv")
)


stopCluster(cl)

registerDoSEQ()

pracma::toc()


### Modelo de regresión Lasso

aptos_x_lasso <- aptos_sin_na
aptos_x_lasso$total_area[is.na(aptos_x_lasso$total_area)] <- mean(aptos_x_lasso$total_area[!is.na(aptos_x_lasso$total_area)])
aptos_x_lasso$covered_area[is.na(aptos_x_lasso$covered_area)] <- mean(aptos_x_lasso$covered_area[!is.na(aptos_x_lasso$covered_area)])
aptos_x_lasso$no_covered_area[is.na(aptos_x_lasso$no_covered_area)] <- mean(aptos_x_lasso$no_covered_area[!is.na(aptos_x_lasso$no_covered_area)])

aptos_x_lasso <- model.matrix(price~.,aptos_x_lasso)


pracma::tic()

no_cores <- detectCores(logical = FALSE)

cl <- makePSOCKcluster(no_cores)

registerDoParallel(cl)

clusterSetRNGStream(cl, iseed=12345)

clusterEvalQ(cl,'myControl')

glmnet_caret <- train(
  x = aptos_x_lasso, 
  y = aptos_y,
  method = 'glmnet',
  trControl = myControl,
  preProcess = "nzv",
  tuneGrid = expand.grid(
    alpha = 0:1,
    lambda = 0:10 / 10
  )
)


stopCluster(cl)

registerDoSEQ()

pracma::toc()

####### RF 

pracma::tic()

no_cores <- detectCores(logical = FALSE)

cl <- makePSOCKcluster(no_cores)

registerDoParallel(cl)

clusterSetRNGStream(cl, iseed=12345)

clusterEvalQ(cl,'myControl')

RF_caret <- train(
  x = aptos_x, 
  y = aptos_y,
  method = 'ranger',
  trControl = myControl,
  preProcess = c("medianImpute", "nzv")
)


stopCluster(cl)

registerDoSEQ()

pracma::toc()


######## RF con imputacion de los factores y agregando maintenance_fee


# Proceso de imputacion factores

# Sacamos variables "basura"

aptos_imput <- aptos %>% select(-c(id,title,accepts_mercadopago,city_name,
                             latitude,longitude,date_created,last_updated,
                             covered_area_unidad,property_type,tipo_cambio,
                             year_month,lat_barrio,fecha_bajada,condition))

# Imputamos item_condition usando RF para predecirla

pracma::tic()

aptos_imput <- imput_fact(datos=aptos_imput,response_y = 'price',modelo='ranger',
                    factor_NA = c('item_condition'))
pracma::toc()

# Proceso para definir Matriz X con item_condition imputado

# vemos prop de NA con imputado

p_na_imput <- sapply(aptos_imput, function(x) round(sum(is.na(x))/length(x),4)) %>% data.frame() %>% 
  rename(prop_na=".") %>% arrange(desc(prop_na))

# Guardamos variables que tiene NA menor a 0.1 y a su vez maintenance_fee

v_NA_imput <- p_na_imput %>% filter(prop_na>0) %>% row.names()

factores_imput <- aptos_imput %>% select_if(~is.factor(.)) 
factores_imput <- factores_imput[,colnames(factores_imput)%in%v_NA_imput] # Sacamos factores que tienen NA


aptos_imput <- aptos_imput %>% select(-c(colnames(factores_imput),
                                    unit_floor, floors, rooms)) # p_na_imput<.1 y maintanece_fee


aptos_x_imput <- aptos_imput %>% select(-price)


# Modelo RF

pracma::tic()

no_cores <- detectCores(logical = FALSE)

cl <- makePSOCKcluster(no_cores)

registerDoParallel(cl)

clusterSetRNGStream(cl, iseed=12345)

clusterEvalQ(cl,'myControl')

RF_imput_caret <- train(
  x = aptos_x_imput, 
  y = aptos_y,
  method = 'ranger',
  trControl = myControl,
  preProcess = c("medianImpute", "nzv")
)


stopCluster(cl)

registerDoSEQ()

pracma::toc()


# Guardamos modelos

save(RF_caret,file='RF_caret.RData')
save(RF_imput_caret,file='RF_imput_caret.RData')
save(lm_caret,file='lm_caret.RData')
save(glmnet_caret,file='glmnet_caret.RData')

