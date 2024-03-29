# Script donde se ajusta :
# 1) Boosting imputando por la media, p_na < .1
# 2) Boosting imputando con MissRanger

# Librerias

library(tidyverse)
library(here)
library(gbm)
library(vip)

options(scipen = 999)

# Funciones auxiliares

source(here("Funciones","funcion_imput_media.R"))

### DATOS

aptos_yearmonth <- list.files(path = here("Datos/Limpios/apt"), 
                              pattern = "*.csv", full.names = T)

yearmonth <- c('aptos_202106','aptos_202107',"aptos_202108",'aptos_202109','aptos_202110')


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

############ Boosting 

set.seed(1234)

ids <- sample(nrow(aptos_sin_na), 0.8*nrow(aptos_sin_na))

train <- aptos_sin_na[ids,]
test <- aptos_sin_na[-ids,]

boosting_train <- gbm::gbm(
  formula = price ~ .,
  data = train,
  distribution='gaussian',
  n.trees = 100L,
  interaction.depth = 1L,
  shrinkage = 0.1
)
  
## Importancia de las variables

pfun <- function(object, newdata) predict(object, newdata = newdata)

importancia_boosting <- vip(boosting_train, method = "permute", target = "price", metric = "rmse", 
         pred_wrapper = pfun,plot=F) 

importancia_boosting$data %>% ggplot(aes(y=reorder(Variable,Importance),x=Importance))+
  geom_col(fill='navyblue')+theme(legend.position="none")+labs(y="Variables",x="Importancia")


save(file="importancia_boosting.RDS",importancia_boosting)

# Veamos RMSE en el conjunto de testeo

RMSE_boosting <- sqrt(mean((test$price-predict(boosting_train,test))^2))


# Guardamos el modelo 

save(file="boosting_train.RDS",boosting_train)


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

boosting_train_mr <- gbm::gbm(
  formula = price ~ .,
  data = train_mr,
  distribution='gaussian',
  n.trees = 100L,
  interaction.depth = 1L,
  shrinkage = 0.1
)

# Importancia de las variables

importancia_boosting_mr <- vip(boosting_train_mr, method = "permute", target = "price", metric = "rmse", 
                            pred_wrapper = pfun,plot=F) 

importancia_boosting_mr$data %>% ggplot(aes(y=reorder(Variable,Importance),x=Importance))+
  geom_col(fill='navyblue')+theme(legend.position="none")+labs(y="Variables",x="Importancia")


save(file="importancia_boosting_mr.RDS",importancia_boosting_mr)

# Veamos RMSE en el conjunto de testeo

RMSE_boosting_mr <- sqrt(mean((test_mr$price-predict(boosting_train_mr,test_mr))^2))

# Guardamos el modelo 

save(file="boosting_train_mr.RDS",boosting_train_mr)
