# Script con funcion para ejecutar CV boosting con(sin) hyperparameter tunning

# Librerias
library(tidyverse)
library(gbm)
library(doParallel)

tunning_boosting <- function(datos,grilla=F,k=10,seed = 1234,parallel = F,control){

  #################### Ajuste grilla ====
  
  # Chequiamos que en la grilla esten todos los hiperametros y sino le ponemos
  # el valor por defecto de la funcion
  
  if(grilla != F){
    
    i.d <- tryCatch( 
              
        expr = {
      
          grilla %>% select(interaction.depth)
        
          }, error = function(cond){
        
            cond <- F
        
            return(F)
        
            }
        
        )
    
    n.t <- tryCatch( 
      
      expr = {
      
        grilla %>% select(n.trees)
      
        }, error = function(cond){
      
          cond <- F
      
          return(F)
      
        }
    
     )
    
  
    s <- tryCatch( 
        
        expr = {
    
          grilla %>% select(shrinkage)
    
          }, error = function(cond){
    
              cond <- F
      
             return(F)
    
            }
  
         )

    n.m <- tryCatch(
  
        expr = {
    
          grilla %>% select(n.minobsinnode)
    
            }, error = function(cond){
    
                cond <- F
    
               return(F)
    
           }
  
          )
  
    
    if(i.d == FALSE){
      
      
      grilla$interaction.depth <- 1
      
      
    } else if(n.t == FALSE){
      
      grilla$n.trees <- 100
      
      
    } else if(s == FALSE){
      
      grilla$shrinkage <- 0.1
      
    } else if(n.m == FALSE){
      
      
      grilla$n.minobsinnode <- 10
      
    }
  

    if(ncol(grilla)!= 4){
      
      
      stop(cat("Chequiar el nombre de los hiperparametros"))
      
    }
    

}
  
  
 ################ Creamos objeto para almacenar el RMSE, Rsquared  y MAE ====
  
  if(grilla == F){
  
  RMSE <- matrix(NA,nrow=1,ncol=k)
  
  Rsquared <- matrix(NA,nrow=1,ncol=k)
  
  MAE <-  matrix(NA,nrow=1,ncol=k)
    
  } else {
    
    RMSE <- matrix(NA,nrow = nrow(grilla),ncol=k)
    
    Rsquared <- matrix(NA,nrow = nrow(grilla),ncol=k)
    
    MAE <- matrix(NA,nrow = nrow(grilla),ncol=k)
    
    
  }
  
  
  ################ Proceso de K-folds ====

    ###### Diferenciamos segun si queremos proceso en paralelo
    
    if(parallel == F){
      
      set.seed(seed)
      
    } else {
      
      print("Comenzo proceso de parallel")
      
      # Calculate the number of cores
      no_cores <- detectCores(logical = FALSE)
      
      cl <- makePSOCKcluster(no_cores)
      
      registerDoParallel(cl)
      
      clusterSetRNGStream(cl, iseed=seed) # Para lograr reproducibilidad
    }
    
   ######## Calculamos el RMSE, Rsquared y MAE  ====
   # para cada Fold y posible combinacion de parametros
    
    for(r in 1:nrow(RMSE)){  
      
      for(c in 1:length(control)){
      
        datos_train <- datos[-c(control[[c]]),]
        
        datos_test <- datos[c(control[[c]]),]
        
        modelo_train <- gbm::gbm(
          formula = price ~ .,
          data = datos_train,
          distribution='gaussian',
          n.trees = grilla$n.trees[r],
          interaction.depth = grilla$interaction.depth[r],
          shrinkage = grilla$shrinkage[r],
          n.minobsinnode = grilla$n.minobsinnode[r]
        )
        
        RMSE[r,c] <- sqrt(mean((datos_test$price-predict(modelo_train,datos_test))^2))

        Rsquared[r,c] <- sum((predict(modelo_train,datos_test)-
                                mean(datos_test$price))^2) /
                        sum((datos_test$price - mean(datos_test$price))^2)
          
        MAE[r,c] <- mean(abs((datos_test$price-predict(modelo_train,datos_test))))
        
      }
        
      }
  
   
   
   #### Si se trabajo con parallel
   
   if(parallel!=F){
     
     ## When you are done:
     stopCluster(cl)
     
     registerDoSEQ() # Para volver a "modo secuencial"
   
     print("Termino proceso de parallel")
     
     }  
    
   ########## Obtenemos el RMSE, Rsquared y MAE promedio para combinacion de parametros
   
   grilla$RMSE <- rowMeans(RMSE)
   
   grilla$Rsquared <- rowMeans(Rsquared)
   
   grilla$MAE <- rowMeans(MAE)
   
   
   ######### Ordenamos segun RMSE, Rsquared y MAE
   
   grilla <- grilla %>% arrange(RMSE,Rsquared,MAE)
   
   ########### Ajustamos el mejor modelo
   
   best <-  gbm::gbm(
     formula = price ~ .,
     data = datos,
     distribution='gaussian',
     n.trees = grilla$n.trees[1],
     interaction.depth = grilla$interaction.depth[1],
     shrinkage = grilla$shrinkage[1],
     n.minobsinnode = grilla$n.minobsinnode[1]
   )
   
   return(resultados = list(tunning = grilla,model = best))
   
   
  
  
}
  
  
