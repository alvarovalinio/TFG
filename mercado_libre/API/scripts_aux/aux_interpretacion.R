# Script auxiliar donde se aplican global model-agnostinc methods (PDP y ALE)

library(here)
library(tidyverse)
#library(iml)
library(ranger)
library(data.table)
library(doParallel)
#library(future)
#library(future.callr)
library(pdp) # PDP plots
library(ALEPlot) #ALE plots
library(gridExtra)

# Cargamos modelo (con mejor performance predictiva)

#best_model <- load(here("mercado_libre/modelos/RF","RF_train.RDS"))

# Por ahora usamos de ejemplo RF con todos los datos (falta correr caret)

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

aptos_sin_na <- aptos_sin_na %>% as.data.table()

#COMENTAR ESTA LINEAL UNA VEZ QUE TENGAMOS LOS MODELOS CON MEJOR AJUSTE
best_model <- ranger(price ~ ., data = aptos_sin_na)

##########################################################
####################### Primer metodo PDP#################
##########################################################

# Primera forma con el paquete pdp

# Proceso de paralelo

# Calculate the number of cores
no_cores <- detectCores(logical = FALSE)

cl <- makePSOCKcluster(no_cores)

registerDoParallel(cl)

clusterSetRNGStream(cl, iseed=1234) # Para lograr reproducibilidad

pracma::tic()

pdp_coverd_area <- partial(best_model, pred.var = "covered_area", quantiles = TRUE, probs = 0:10/10,
        ice = FALSE, center = FALSE, plot = TRUE, plot.engine = "ggplot2")

pdp_dist_shop<- partial(best_model, pred.var = "dist_shop", quantiles = TRUE, probs = 0:10/10,
                           ice = FALSE, center = FALSE, plot = TRUE, plot.engine = "ggplot2")

pdp_dist_rambla <- partial(best_model, pred.var = "dist_rambla", quantiles = TRUE, probs = 0:10/10,
                           ice = FALSE, center = FALSE, plot = TRUE, plot.engine = "ggplot2")
## When you are done:
stopCluster(cl)

registerDoSEQ() # Para volver a "modo secuencial"

pracma::toc()

##########################################################
####################### Segundo metodo ALE#################
##########################################################

## Mediante ALEplot

# Definimos funcion para hacer predicciones
yhat <- function(X.model, newdata) as.numeric(ranger::predictions(predict(X.model, newdata)))

## OBS, definir diferente yhat si best_modelo != RF (sacar ranger::predictions)

## Definimos data.frame con las variables de entrada

X <- aptos_sin_na[,-c("price")]

### Graficos 

ale_covered_area <- ALEPlot(X,best_model,yhat,J=2,NA.plot=T)

ale_dist_shop <- ALEPlot(X,best_model,yhat,J=22,NA.plot=T)

ale_dist_rambla <- ALEPlot(X,best_model,yhat,J=23,NA.plot=T)


g_covered_area <- cbind(ale_covered_area$x.values,ale_coverd_area$f.values) %>% data.frame() %>% 
  ggplot(aes(x=X1,y=X2)) + geom_line() +labs(x="covered_area",y="ALE",title="ALE covered_area")


g_dist_shop <- cbind(ale_dist_shop$x.values,ale_coverd_area$f.values) %>% data.frame() %>% 
  ggplot(aes(x=X1,y=X2)) + geom_line() +labs(x="dist_shop",y="ALE",title="ALE dist_shop")

g_dist_rambla <- cbind(ale_covered_area$x.values,ale_dist_rambla$f.values) %>% data.frame() %>% 
  ggplot(aes(x=X1,y=X2)) + geom_line() +labs(x="dist_rambla",y="ALE",title="ALE dist_rambla")



######## Vemos graficamente ambos metodos

#######################################################################
# Definimos matriz de variables de entrada

X <-  aptos_sin_na %>% select(-price) %>% data.table()

# Definimos funcion para obtener la predicciones (¿solo necesario en ranger?)

pfun <- function(object, newdata) predict(object, data = newdata)$predictions 

## Contruimos el objeto para hacer la prediccion

model <-  Predictor$new(model =  best_model, data = X, y = aptos_sin_na$price,
                        predict.fun = pfun)

# PDP

pdp <- Partial$new(model,"covered_area", ice=FALSE)

plot(pdp)

## Proceso de parallalel
library(bench)

system_time({
# Creates a PSOCK cluster with 2 cores
plan("callr", workers = 2)

## Efecto covered_area

eff_covered_area <- FeatureEffect$new(model, feature = "covered_area", 
                         method = 'pdp', grid.size = 30)

plan("callr", workers = 2)

plot(eff_covered_area)


})


system_time({
  
  plan("sequential")
  
  ## Efecto covered_area
  
  eff_covered_area <- FeatureEffect$new(model, feature = "covered_area", 
                                        method = 'pdp', grid.size = 30)
  
  plan("sequential")
  
  plot(eff_covered_area)
  
  
})



## Efecto de full_bathrooms

eff_full_bathrooms<- FeatureEffect$new(model, feature = "full_bathrooms", 
                                      method = 'pdp')


plot(eff_full_bathrooms)



##### ALE

eff_full_bathrooms_ale<- FeatureEffect$new(model, feature = "full_bathrooms")


plot(eff_full_bathrooms_ale)
