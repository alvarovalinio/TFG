# Script donde se realiza la imputación por la media 

# Parametros de entrada 1) datos y 2) proporcion de NAs "tolerada"

imput_media <- function(datos,p=.1){
  
  
  # vemos prop de NA
  p_na <- sapply(datos, function(x) round(sum(is.na(x))/length(x),4)) %>% data.frame() %>% 
    rename(prop_na=".") %>% arrange(desc(prop_na))
  
  # Guardamos variables que tienen más de 10% de NA
  
  v_NA <- p_na %>% filter(prop_na>p) %>% row.names()
  
  # Guardamos variables factores a sacar
  
  v_NA_f <- datos %>% select(p_na %>% filter(prop_na > 0 , prop_na <= p) %>% row.names()) %>%
    select_if(is.factor) %>% colnames()
  
  # Guardamos variables a imputar por la media (criterio ver)
  
  imput_media <- datos %>% select(p_na %>% filter(prop_na > 0 , prop_na <= p) %>% row.names()) %>%
    select_if(is.numeric) %>% colnames()
  
  # Imputación por la media
  
  # Objeto para almacenar
  
  datos_sin_na <- datos
  
  if(length(imput_media)!=0){
    
    im <- function(datos,columna){
      
      datos[,columna][is.na(datos[,columna])] <- mean(datos[,columna][!is.na(datos[,columna])])
      
      return(datos)
      
    }
    
    
    for(i in imput_media){
      
      datos_sin_na <- im(datos_sin_na,i)
      
    }
    
  }
  
  datos_sin_na <- datos_sin_na %>% select(-v_NA,-v_NA_f,
                                          -c(id,title,accepts_mercadopago,city_name,
                                             latitude,longitude,date_created,last_updated,
                                             property_type,tipo_cambio,year_month,lat_barrio,
                                             fecha_bajada))
  
  
  
  return(datos_sin_na)
  
  
  
}