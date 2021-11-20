# Script auxiliar donde se aplican global model-agnostinc methods

library(here)
library(tidyverse)
library(iml) 
library(data.table)
library(doParallel)
library(gridExtra)
library(vip)
library(viridis)


# Cargamos modelo (con mejor performance predictiva)

load(here("Modelos/Resultados/BOOSTING/caret","Boosting_caret_tunning.RDS"))

best_model <- Boosting_caret_tunning[[2]]

# Por ahora usamos de ejemplo RF con todos los datos (falta correr caret)

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

aptos_sin_na <- aptos_sin_na %>% as.data.table()

###########################################################
################## Primer metodo permutation ##############
###########################################################

pfun <- function(object, newdata) predict(object, newdata = newdata)

set.seed(1234)

importancia_best <- vip(Boosting_caret_tunning$model, method = "permute", target = "price", metric = "rmse", 
                            pred_wrapper = pfun,plot=F,train=aptos_sin_na) 

importancia_best$data %>% ggplot(aes(y=reorder(Variable,Importance),x=Importance))+
  geom_col(fill='navyblue')+theme(legend.position="none")+labs(y="Variables",x="Importancia")

save(file="importancia_best.RDS",importancia_best)


##########################################################
####################### Segundo metodo PDP#################
##########################################################

# Definimos matriz de variables de entrada

X <-  aptos_sin_na %>% select(-price) %>% data.table()

# Definimos funcion para obtener la predicciones

pfun <- function(X.model, newdata) as.numeric(gbm::predict.gbm(X.model, newdata))

## Contruimos el objeto para hacer la prediccion

model <-  Predictor$new(model =  best_model, data = X, y = aptos_sin_na$price,
                        predict.fun = pfun)

### Construimos diferentes objetos para pdp

## Variables individuales

pdp_total_area <- FeatureEffect$new(model, feature = "total_area", 
                                      method = 'pdp')

pdp_total_area$results %>% ggplot(aes(x=total_area,y=.value)) + geom_point()+
  geom_line()


save(file="pdp_total_area.RDS",pdp_total_area)


pdp_dist_shop <- FeatureEffect$new(model, feature = c("dist_shop"), 
                                      method = 'pdp')

pdp_dist_shop$results %>% ggplot(aes(x=dist_shop,y=.value)) + geom_point()+
  geom_line()


save(file="pdp_dist_shop.RDS",pdp_dist_shop)

pdp_dist_rambla <- FeatureEffect$new(model, feature = "dist_rambla", 
                                      method = 'pdp')

pdp_dist_rambla$results %>% ggplot(aes(x=dist_rambla,y=.value)) + geom_point()+
  geom_line()


save(file="pdp_dist_rambla.RDS",pdp_dist_rambla)


## full_bathrooms

pdp_full_bathrooms <- FeatureEffect$new(model, feature = "full_bathrooms", 
                                     method = 'pdp')

pdp_full_bathrooms$results %>% ggplot(aes(x=full_bathrooms,y=.value)) + geom_col()

save(file="pdp_full_bathrooms.RDS",pdp_full_bathrooms)

## Interaccion

## Distancia rambla y fullbathrooms

pdp_dr_fb <- FeatureEffect$new(model, feature = c("dist_rambla","full_bathrooms"), 
                                     method = 'pdp')

pdp_dr_fb$results %>% ggplot(aes(x=dist_rambla,y=.value,color=full_bathrooms)) + geom_point()+
  geom_line()

save(file="pdp_dr_fb.RDS",pdp_dr_fb)

## Distancia shop y bedrooms

pdp_ds_b <- FeatureEffect$new(model, feature = c("dist_shop","bedrooms"), 
                               method = 'pdp')

pdp_ds_b$results %>% ggplot(aes(x=dist_shop,y=.value,color=bedrooms)) + geom_point()+
  geom_line()


## Total area y ingreso medio ECH

pdp_ta_ech <- FeatureEffect$new(model, feature = c("total_area","ingresomedio_ech"), 
                              method = 'pdp')

pdp_ta_ech$results %>% ggplot(aes(x=total_area,y=ingresomedio_ech,
                                  fill=.value)) + geom_tile() +
  scale_fill_viridis(discrete =FALSE) 


save(file="pdp_ta_ech.RDS",pdp_ta_ech)

