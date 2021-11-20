# Script donde se ajusta :
# 1) RF imputando por la media, p_na < .1
# 2) RF imputando con MissRanger

# Librerias

library(tidyverse)
library(here)
library(ranger)
library(missRanger)
library(vip)

options(scipen = 999)

# Funciones auxiliares

source(here("Funciones","funcion_imput_media.R"))

### DATOS

aptos_yearmonth <- list.files(path = here("Datos/Limpios/apt"), 
                              pattern = "*.csv", full.names = T)

yearmonth <- c('aptos_202106','aptos_202107',"aptos_202108","aptos_202109","aptos_202110")


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

rf_train <- ranger(price ~ ., data = train,
             importance = 'impurity')

rf_train # Vemos el OOB error entre otras caracteristicas relevantes

 # Importancia de las variables (Primera forma segun OOB)

# importancia_rf <- data.frame(rf_train$variable.importance)
# 
# colnames(importancia_rf) <- c("importance")
# 
# importancia_rf$variables <- row.names(importancia_rf)
# 
# importancia_rf  %>% ggplot(aes(y=reorder(variables,importance),x=importance,fill=variables))+
#   geom_col()+theme(legend.position="none")+labs(y="Variables",x="Importancia")


# Importancia de las variables (Permuted)

pfun_RF <- function(object, newdata) predictions(predict(object,newdata))

importancia_rf <- vip(rf_train, method = "permute", target = "price", metric = "rmse", 
                            pred_wrapper = pfun_RF,plot=F) 

save(file="importancia_rf.RDS",importancia_rf)

importancia_rf$data %>% ggplot(aes(y=reorder(Variable,Importance),x=Importance))+
  geom_col(fill='navyblue')+theme(legend.position="none")+labs(y="Variables",x="Importancia")


# Veamos RMSE en el conjunto de testeo

RMSE_rf <- sqrt(mean((test$price-predictions(predict(rf_train,test)))^2))

# Guardamos el modelo 

save(file="RF_train.RDS",rf_train)


#### Veamos imputación por missranger

### Imputamos las variables anteriores por miss ranger

# Cargamos los datos que estan imputados con missRanger 

aptos_mr <- read_csv(here("Datos/Limpios/apt/aptos_mr","aptos_mr.csv"))

aptos_mr <- aptos_mr %>% mutate_if(is.character, as.factor)

# Definimos mismo conjunto de entrenamiento y testeo

train_mr <- aptos_mr[ids,]
test_mr <- aptos_mr[-ids,]


# Ajustamos modelo

set.seed(1234)

rf_train_mr <- ranger(price ~ ., data = train_mr,
             importance = 'impurity')

rf_train_mr # Vemos el OOB error entre otras caracteristicas relevantes

# Importancia de las variables

# importancia_rf_mr <- data.frame(rf_train_mr$variable.importance)
# 
# colnames(importancia_rf_mr) <- c("importance")
# 
# importancia_rf_mr$variables <- row.names(importancia_rf_mr)
# 
# importancia_rf_mr  %>% ggplot(aes(y=reorder(variables,importance),x=importance,fill=variables))+
#   geom_col()+theme(legend.position="none")+labs(y="Variables",x="Importancia")


importancia_rf_mr <- vip(rf_train_mr, method = "permute", target = "price", metric = "rmse", 
                      pred_wrapper = pfun_RF,plot=F) 

save(file="importancia_rf_mr.RDS",importancia_rf_mr)

importancia_rf$data %>% ggplot(aes(y=reorder(Variable,Importance),x=Importance))+
  geom_col(fill='navyblue')+theme(legend.position="none")+labs(y="Variables",x="Importancia")


# Veamos RMSE en el conjunto de testeo

RMSE_rf_mr <- sqrt(mean((test_mr$price-predictions(predict(rf_train_mr,test_mr)))^2))


# Guardamos el modelo 

save(file="RF_train_mr.RDS",rf_train_mr)

