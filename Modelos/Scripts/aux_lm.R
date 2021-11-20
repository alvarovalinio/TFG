# Script donde se ajusta un modelo de regresion lineal multiple,
# teniendo en cuenta ambos metodos de imputacion

# Librerias

library(tidyverse)
library(here)
library(broom)
library(lmtest)
library(nortest)


options(scipen = 999)

# Funciones auxiliares

source(here("Funciones","funcion_imput_media.R"))

### DATOS

aptos_yearmonth <- list.files(path = here("Datos/Limpios/apt"), 
                              pattern = "*.csv", full.names = T)

yearmonth <- c('aptos_202106','aptos_202107',"aptos_202109","aptos_202110")


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


############ LM 

set.seed(1234)
ids <- sample(nrow(aptos_sin_na), 0.8*nrow(aptos_sin_na))

train <- aptos_sin_na[ids,]
test <- aptos_sin_na[-ids,]

lm <- lm(price ~ ., data = train)

# Resumen del modelo

tidy(lm)

glance(lm)


# Model checking

#Homoscedasticidad

ggplot(as_tibble(lm$residuals) %>% 
             mutate(id = seq(1, length(lm$residuals), 1))) + 
      geom_point(aes(x = id, y = value), color = 'red', alpha = 1/5)

lmtest::bptest(lm) # se rechaza h0 de varianza constante 

#Normalidad

ggplot(as_tibble(lm$residuals)) + 
      geom_density(aes(value), color = 'red')

lillie.test(residuals(lm)) #se rechaza h0 de normalidad

### Error de predicción

RMSE_lm <- sqrt(mean((test$price - predict(lm,test))^2))

MAE_lm <- mean(abs(test$price-predict(lm,test)))


#### Veamos imputación por missranger

### Imputamos las variables anteriores por miss ranger

# Cargamos los datos que estan imputados con missRanger 

aptos_mr <- read_csv(here("Datos/Limpios/apt/aptos_mr","aptos_mr.csv"))

aptos_mr <- aptos_mr %>% mutate_if(is.character, as.factor)

# Definimos mismo conjunto de entrenamiento y testeo

train_mr <- aptos_mr[ids,]
test_mr <- aptos_mr[-ids,]


lm_mr <- lm(price ~ ., data = train_mr)

# Resumen del modelo

tidy(lm_mr)

glance(lm_mr)


# Model checking

#Homoscedasticidad

ggplot(as_tibble(lm_mr$residuals) %>% 
          mutate(id = seq(1, length(lm$residuals), 1))) + 
   geom_point(aes(x = id, y = value), color = 'red', alpha = 1/5)

lmtest::bptest(lm_mr) # se rechaza h0 de varianza constante 

#Normalidad

ggplot(as_tibble(lm$residuals)) + 
   geom_density(aes(value), color = 'red')

lillie.test(residuals(lm_mr)) #se rechaza h0 de normalidad

### Error de predicción

RMSE_lm_mr <- sqrt(mean((test_mr$price - predict(lm_mr,test_mr))^2))

MAE_lm_mr <- mean(abs(test_mr$price-predict(lm_mr,test_mr)))
