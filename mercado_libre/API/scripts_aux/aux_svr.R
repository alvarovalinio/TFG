# Script donde se ajusta :
# 1) SVR utilizando Radial Kernel imputando por la media, p_na < .1
# 2) SVR utlizando Radial Kernel imputando con MissRanger

# Librerias

library(tidyverse)
library(here)
library(kernlab)
library(missRanger)

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


#### Definimos variables Sin na

aptos_sin_na <- imput_media(aptos,p=.1)

############ SVR 

set.seed(1234)
ids <- sample(nrow(aptos_sin_na), 0.8*nrow(aptos_sin_na))

train <- aptos_sin_na[ids,]
test <- aptos_sin_na[-ids,]

# Variables de entrada
 
SVR_train <-
  kernlab::ksvm(
    price ~ .,
    data = train,
    scaled = TRUE,
    C = 1,
    kernel = "rbfdot",
    kpar = list(sigma = 1),
    type = "eps-svr",
    epsilon = 0.1
  )


# Vemos el numero de support vectors

SVR_train

# Veamos el Error de prediccion 

RMSE_svr <- sqrt(mean((test$price-predict(SVR_train,test))^2))

RMSE_svr

################

# Guardamos el modelo 

save(file="SVR_train.RDS",SVR_train)


#### Veamos imputación por missranger

### Imputamos las variables anteriores por miss ranger

# Cargamos los datos que estan imputados con missRanger 

aptos_mr <- read_csv(here("mercado_libre/API/datos/limpios/apt","aptos_mr.csv"))

aptos_mr <- aptos_mr %>% mutate_if(is.character, as.factor)

# Definimos mismo conjunto de entrenamiento y testeo

train_mr <- aptos_mr[ids,]
test_mr <- aptos_mr[-ids,]


# Ajustamos modelo

set.seed(1234)

SVR_train_mr <-
  kernlab::ksvm(
    price ~ .,
    data = train_mr,
    scaled = TRUE,
    C = 1,
    kernel = "rbfdot",
    kpar = list(sigma = 1),
    type = "eps-svr",
    epsilon = 0.1
  )


# Vemos el numero de support vectors

SVR_train_mr

# Veamos RMSE en el conjunto de testeo

RMSE_svr_mr <- sqrt(mean((test_mr$price-predict(SVR_train_mr,test))^2))


# Guardamos el modelo 

save(file="SVR_train_mr.RDS",SVR_train_mr)