# Script donde se trabaja con arboles de regresion tanto
# para imputacion por la media como por missranger

rm(list=ls())

library(tidyverse)
library(here)
library(caret)
library(doParallel)
#library(pracma)
library(rpart)
library(rpart.plot)
library(rattle)
library(ranger)

options(scipen = 999)

# Funciones auxiliares

source(here("mercado_libre/API/funciones","funcion_imput_media.R"))

#### DATOS

aptos_yearmonth <- list.files(path = here("mercado_libre/API/datos/limpios/apt"), 
                              pattern = "*.csv", full.names = T)

yearmonth <- c('aptos_202106','aptos_202107',"aptos_202108","aptos_202109","aptos_202110")

aptos <- sapply(aptos_yearmonth, FUN=function(yearmonth){
      read_csv(file=yearmonth)}, simplify=FALSE) %>% bind_rows()


aptos <- aptos %>% group_by(id) %>% 
      arrange(desc(fecha_bajada)) %>%
      slice(1) %>% ungroup()

aptos <- aptos %>% mutate_if(is.character, as.factor)

# Filtramos por el criterio en price - eliminamos obs. con price superior al percentil 95%

aptos_todos <- aptos

aptos <- aptos %>% filter(price <= quantile(aptos$price,.95))

# Perdemos esta cantidad de registros

nrow(aptos_todos) - nrow(aptos)

# vemos prop de NA
p_na <- sapply(aptos, function(x) round(sum(is.na(x))/length(x),4)) %>% data.frame() %>% 
      rename(prop_na=".") %>% arrange(desc(prop_na))

#### Definimos variables Sin na

aptos_sin_na <- imput_media(aptos,p=.1)

### Imputacion por media

##### Arbol de regresion

set.seed(1234)
ids <- sample(nrow(aptos_sin_na), 0.8*nrow(aptos_sin_na))

train <- aptos_sin_na[ids,]
test <- aptos_sin_na[-ids,]

set.seed(1234)
arbol <- rpart(price~ . , data=train)

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

arbol.prune <- rpart(price ~ . , data = train,method="anova",
                     control=rpart.control(cp=cp_opt))

# Grafico

rpart.plot(arbol.prune,roundint = T,digits = 4)

#### Imputacion por missranger

# Cargamos los datos que estan imputados con missRanger 

aptos_mr <- read_csv(here("mercado_libre/API/datos/limpios/apt/aptos_mr","aptos_mr.csv"))

aptos_mr <- aptos_mr %>% mutate_if(is.character, as.factor)

# Definimos mismo conjunto de entrenamiento y testeo

train_mr <- aptos_mr[ids,]
test_mr <- aptos_mr[-ids,]

set.seed(1234)
arbol_mr <- rpart(price~ . , data=train_mr)

summary(arbol_mr)

# Proceso de poda

broom::tidy(arbol_mr$cptable)

# Grafico de la evolucion del error

cp_error_mr <- data.frame(arbol_mr$cptable)

cp_error_mr %>% ggplot(aes(x=CP,y=xerror))+geom_point(color="red")+geom_line()

# Otra forma

plotcp(arbol_mr) 

# Obtengamos el cp

cp_opt_mr <- arbol_mr$cptable[which.min(arbol_mr$cptable[,"xerror"]),"CP"]

# Arbol podado:

arbol.prune.mr <- rpart(price ~ . , data = train_mr,method="anova",
                     control=rpart.control(cp=cp_opt_mr))

# Grafico

rpart.plot(arbol.prune.mr,roundint = T,digits = 4)



## Guardamos los modelos

save(file="arbol_train.RDS",arbol)

save(file="arbol_prune_train.RDS",arbol.prune)

save(file="arbol_train_mr.RDS",arbol_mr)

save(file="arbol_prune_train_mr.RDS",arbol.prune.mr)




