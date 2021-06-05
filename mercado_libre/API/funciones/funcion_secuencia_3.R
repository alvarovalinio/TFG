# Script donde se guarda una funcion para detectar si hay una secuencia de
# largo de 3 de numeros repetidos

secuencia_3 <- function(numero){
  
  if(numero<100){
    
    stop("El numero de menor que 100")
    
  } else {
    
    decimal <- 10
    
    stop <- 0
    
    resultado <- "No"
    
    while(decimal<numero & stop==0){
      
    if(numero%%decimal==floor(numero/decimal)%%decimal &
       floor(numero/decimal)%%decimal==floor(numero/(decimal*10))%%decimal){
      
      if(numero%%decimal!=0){
      
      resultado <- "Si"
      
      stop <- 1
      
      } else{
        
        numero <- floor(numero/decimal)
        
      }
      
      
      
    } else {
      
      numero <- floor(numero/decimal)
      
      
    }
      
      
      
    }
    
    return(resultado)
    
    
    
  }
  
  
}
