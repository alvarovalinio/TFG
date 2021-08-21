# Script donde se ajusta :
# 1) RF imputando por la media, p_na < .1
# 2) RF imputando con MissRanger

# Librerias

library(tidyverse)
library(here)
library(ranger)
library(missRanger)

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


#### Definimos variables Sin na

aptos_sin_na <- imput_media(aptos,p=.1)

############ RF 

set.seed(1234)
ids <- sample(nrow(aptos_sin_na), 0.8*nrow(aptos_sin_na))

train <- aptos_sin_na[ids,]
test <- aptos_sin_na[-ids,]

rf <- ranger(price ~ ., data = train,
             importance = 'impurity')

 # Importancia de las variables

importancia_rf <- data.frame(rf$variable.importance)

colnames(importancia_rf) <- c("importance")

importancia_rf$variables <- row.names(importancia_rf)

importancia_rf  %>% ggplot(aes(y=reorder(variables,importance),x=importance,fill=variables))+
  geom_col()+theme(legend.position="none")+labs(y="Variables",x="Importancia")


# Veamos RMSE en el conjunto de testeo

RMSE_rf <- sqrt(mean((test$price-predictions(predict(rf,test)))^2))

#### Veamos imputación por missranger

### Imputamos las variables anteriores por miss ranger
 

aptos_mr <- missRanger(aptos %>% select(names(aptos_sin_na),-price), 
                       pmm.k = 3, num.trees = 100,seed=1234)

aptos_mr$price <- aptos_sin_na$price

set.seed(1234)
ids <- sample(nrow(aptos_mr), 0.8*nrow(aptos_mr))

train <- aptos_mr[ids,]
test <- aptos_mr[-ids,]

rf_mr <- ranger(price ~ ., data = train,
             importance = 'impurity')

# Importancia de las variables

importancia_rf_mr <- data.frame(rf_mr$variable.importance)

colnames(importancia_rf_mr) <- c("importance")

importancia_rf_mr$variables <- row.names(importancia_rf_mr)

importancia_rf_mr  %>% ggplot(aes(y=reorder(variables,importance),x=importance,fill=variables))+
  geom_col()+theme(legend.position="none")+labs(y="Variables",x="Importancia")


# Veamos RMSE en el conjunto de testeo

RMSE_rf_mr <- sqrt(mean((test$price-predictions(predict(rf_mr,test)))^2))

#### Veamos imputación por missranger