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
p_na <- sapply(aptos, function(x) round(sum(is.na(x))/length(x),3)) %>% data.frame() %>% 
  rename(prop_na=".") %>% arrange(desc(prop_na))

# Guardamos variables que tiene NA

v_NA <- p_na %>% filter(prop_na>0) %>% row.names()

##### Arbol de regresion

set.seed(12345)

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



#### CARET

# Proceso de imputacion factores

# Sacamos variables "basura"

aptos <- aptos %>% select(-c(id,title,accepts_mercadopago,city_name,
                             latitude,longitude,date_created,last_updated,
                             covered_area_unidad,property_type,tipo_cambio,
                             year_month,lat_barrio,fecha_bajada,condition))
pracma::tic()

aptos <- imput_fact(datos=aptos,response_y = 'price',modelo='ranger',
                    factor_NA = c('item_condition','tags'))
pracma::toc()

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

# Matriz X

# Dejamos property_age con 42% de Na 
aptos_x <- aptos %>% select(-c(p_na %>% filter(prop_na>0) %>% row.names(),
                              id,price,title,accepts_mercadopago,city_name,
                              latitude,longitude,date_created,last_updated,
                              covered_area_unidad,property_type,tipo_cambio,
                              year_month,lat_barrio,fecha_bajada,condition)) 



# Modelo
tic()
model <- train(
  y = aptos_y,
  x = aptos_x,
  method = "ranger",
  trControl = myControl
   # preProcess = "knnImpute"
)
toc()

