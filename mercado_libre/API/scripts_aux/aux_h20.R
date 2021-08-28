# Librerias

library(tidyverse)
library(here)
library(ranger)
library(missRanger)
library(caret)
library(h2o)


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

# Definimos la martiz x de variables regresoras
aptos_x <- aptos_sin_na %>% select(-price)

h2o.init()

aptos_h2o <- as.h2o(aptos_sin_na)

aptos_y <- "price"
aptos_x <- setdiff(colnames(aptos_h2o), aptos_y)

# Training, validation and test sets

set.seed(12345)
# Split data into train & validation sets
sframe <- h2o.splitFrame(aptos_h2o, seed = 42)
train <- sframe[[1]]
valid <- sframe[[2]]

# MODELING
# Train random forest model
rf_model <- h2o.randomForest(x = aptos_x,
                             y = aptos_y,
                             training_frame = train,
                             validation_frame = valid)

# Calculate model performance
perf <- h2o.performance(rf_model, valid = TRUE)      


# IMPLEMENTAMOS AUTO MACHINE LEARNING
automl_model <- h2o.automl(x = aptos_x, y = aptos_y,
                           training_frame = train,
                           validation_frame = valid,
                           max_runtime_secs = 600,
                           sort_metric = "RMSE",
                           seed = 42)

lb <- automl_model@leaderboard

# List all models by model id
model_ids <- as.data.frame(lb)$model_id

# Get the best model
aml_leader <- automl_model@leader
