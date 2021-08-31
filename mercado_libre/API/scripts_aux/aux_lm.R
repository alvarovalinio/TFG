# Script donde se ajusta :
# lm, mcp, lasso imputando por la media, p_na < .1

# Librerias

library(tidyverse)
library(here)
library(ranger)
library(missRanger)
library(lmtest)
library(nortest)
library(glmnet)

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


############ LM 

set.seed(1234)
ids <- sample(nrow(aptos_sin_na), 0.8*nrow(aptos_sin_na))

train <- aptos_sin_na[ids,]
test <- aptos_sin_na[-ids,]

lm <- lm(price ~ ., data = train)

summary(lm)

# Model checking

#Homoscedasticidad

ggplot(as_tibble(lm$residuals) %>% 
             mutate(id = seq(1, length(lm$residuals), 1))) + 
      geom_point(aes(x = id, y = value), color = 'red', alpha = 1/5)

lmtest::bptest(lm) # se rechaza h0 de varianza constante 
# hay patrones que el modelo no longró captar -> no se cumple homoscedasicidad

#Normalidad

ggplot(as_tibble(lm$residuals)) + 
      geom_density(aes(value), color = 'red')

lillie.test(residuals(lm)) #se rechaza h0 de normalidad

# Residuos

RMSE_lm <- sqrt(mean((test$price - predict(lm,test))^2))

# probamos aplicar logaritmos en price, empeora error cuadrático medio
# no se soluciona el problema de heteroscedasticidad ni no normalidad de los errores


# MINIMOS CUADRADOS PONDERADOS

#define weights to use
wt <- 1 / lm(abs(lm$residuals) ~ lm$fitted.values)$fitted.values^2

#perform weighted least squares regression
wls_model <- lm(price ~ ., data = train, weights=wt)

summary(wls_model)

lmtest::bptest(wls_model) # no se soluciona
lillie.test(residuals(lm)) # no se soluciona

# Residuos

RMSE_wls <- sqrt(mean((test$price - predict(wls_model,test))^2))

# Regresión penalizada - Lasso

aptos_x_lasso <- model.matrix(price~.,train)
aptos_y <- train$price

aptos_x_lasso_test <- model.matrix(price~.,test)
aptos_y_test <- test$price

glmnet <- glmnet(aptos_x_lasso, aptos_y, family = 'gaussian')

glmnet$beta

RMSE_glmnet <- sqrt(mean((test$price - predict.glmnet(glmnet,aptos_x_lasso_test))^2))





