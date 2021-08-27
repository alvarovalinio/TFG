
# Librerias

library(tidyverse)
library(here)
library(ranger)
library(missRanger)
library(caret)

options(scipen = 999)

# Funciones auxiliares

source(here("mercado_libre/API/funciones","funcion_imput_media.R"))

### DATOS

aptos_yearmonth <- list.files(path = here("mercado_libre/API/datos/limpios/apt"), 
                              pattern = "*.csv", full.names = T)

yearmonth <- c('aptos_202106','aptos_202107',"aptos_2018")


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


#################################################################
# Caso 1: imputamos solo variables cuantitativas con % NA < 0.1 #
#################################################################

#### Definimos variables Sin na

aptos_sin_na <- imput_media(aptos,p=.1)

# Create custom indices: myFolds

aptos_y <- aptos_sin_na$price

set.seed(12345)

myFolds <- createFolds(aptos_y, k = 10)

# Create reusable trainControl object: myControl
myControl <- trainControl(
      verboseIter = TRUE,
      savePredictions = TRUE,
      index = myFolds
)

# Definimos la martiz x de variables regresoras
aptos_x <- aptos_sin_na %>% select(-price)

# 1er - Modelo Random Forest
RF_caret <- train(
      x = aptos_x, 
      y = aptos_y,
      method = 'ranger',
      trControl = myControl,
      preProcess = c("nzv")
)

# Vemos graficamente el comportamiento de los hiperparametros
plot(RF_caret)

# Busqueda orientada en el espacio de los hiperparametros
# (adaptive resampling)

########################################
# Caso 2: imputamos usando Miss Ranger #
########################################

# Imputamos usando missRanger

aptos_imputar <- aptos %>% select(-c(id,title,accepts_mercadopago,city_name,
                                     latitude,longitude,date_created,last_updated,
                                     covered_area_unidad,property_type,tipo_cambio,
                                     year_month,lat_barrio,fecha_bajada,condition))

# sacamos price para que no entre como covariable

aptos_imputado <- missRanger(aptos_imputar %>% select(-price), 
                             pmm.k = 3, num.trees = 100)

aptos_x_imput <- aptos_imputado %>% select(-price)

aptos_y_imput <- aptos_imputado$price 

# 2do - Modelo Random Forest
RF_caret_missr <- train(
      x = aptos_x_imput, 
      y = aptos_y_imput,
      method = 'ranger',
      trControl = myControl,
      preProcess = c("nzv")
)

# Vemos graficamente el comportamiento de los hiperparametros
plot(RF_caret)

# Busqueda orientada en el espacio de los hiperparametros
# (adaptive resampling)


