# Script para imputar los NA en factores, utilizando caret para realizar
# predicciones

# Parametros: 1) datos, 2)response_y = variable respuesta, 3)factor_NA =FALSE
# por defecto es FALSE, lo cual quiere decir que imputa todos. Por otro lado
# le podemos pasar un vector con el nombre de las variables que queremos imputar

library(tidyverse)
library(caret)
library(doParallel)

imput_fact <- function(datos,response_y,modelo,factor_NA=FALSE){
  
    
# Obtenemos variables con NA
  v_NA <- sapply(datos, function(x) round(sum(is.na(x))/length(x),3)) %>% data.frame() %>% 
  rename(prop_na=".") %>% arrange(desc(prop_na))  %>% filter(prop_na>0) %>% row.names()


if(factor_NA==FALSE){ # Si queremos todos los factores
    
  
# Obtenemos los factores que tienen NA

factores <- datos %>% select_if(~is.factor(.)) 

factores <- factores[,colnames(factores)%in%v_NA]

} else{
  
  factores <- datos %>% select(factor_NA)
  
  
}

# Agregamos variable id para mapear

factores$id <- seq(1,nrow(factores)) 

factores <- factores %>% relocate(id)


# Definimos la matriz X (Todas las variables que no tienen NA ni response_y)

datos_x <- datos %>% select(-c(all_of(v_NA),all_of(response_y))) 

# Por cada columna(menos id) de factores tenemos que ajustar un modelo y luego 
# predecir los NA

for(i in 2:ncol(factores)){
  
  
  datos_y <- factores[,i] %>% filter(!is.na(.)) # Response not missing
  
  datos_y <- factor(datos_y[[1]],levels=levels(factores[[i]]))
  
  datos_pred <- factores[,c(1)] %>% filter(is.na(factores[,i])) # Response missing 
  
  # Tomamos los datos_x que no estan en datos_pred por id
  
  datos_x_aux <- datos_x[-c(datos_pred$id),]
  
  # Create reusable trainControl object: myControl
  
  fitControl <- trainControl(method = "adaptive_cv",
                             number = 3, repeats = 3,
                             adaptive = list(min=3, alpha = 0.05, method = 'BT', complete = FALSE),
                             search = "random")
  
 # Proceso en paralelo
  
# Fit random forest: model_rf
  cl <- makePSOCKcluster(5)
  registerDoParallel(cl)
  
  ## All subsequent models are then run in parallel
  model_rf <- train(
    x = datos_x_aux, 
    y = datos_y,
    method = modelo,
    trControl = fitControl,
    preProcess = c('nzv')
  )
  
  
  ## When you are done:
  stopCluster(cl)
  
  datos_pred$pred <- predict(model_rf,datos[datos_pred$id,]) # Falta setiar que este el ID
  
  # Reemplazamos los NA
  
  datos[datos_pred$id,colnames(factores)[i]] <- datos_pred$pred
 
  print(paste0("Termino el factor: ", names(factores)[i]))
   
}

# Obtenemos predicciones de los Na

return(datos)



}