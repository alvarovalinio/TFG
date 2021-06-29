# Funcion para mapear barrios datos - barrios INE

library(here)
library(data.table)
library(tidyverse)
library(lubridate)
library(sf) # Mapas
library(RecordLinkage) #funcion para match palabras



shops_barrios <- function(datos_entrada){
  
  
  #Leemos vctorial INE
  mapa_barrio <- st_read("mercado_libre/API/scripts_aux/Mapas/vectorial_INE_barrios/ine_barrios")
  #Pasa geometría a formato longlat 
  mapa_barrio <- st_transform(mapa_barrio, '+proj=longlat +zone=21 +south +datum=WGS84 +units=m +no_defs')
  
  ### match city names - mapa barrio
  aptos_city <- levels(as.factor(datos_entrada$city_name))
  aptos_city <- as.data.table(aptos_city) %>% 
    rename(city_name = aptos_city) %>%
    mutate(NOMBBARR = 'no match') %>%
    mutate(city_name2 = city_name)
  
  
  
  for(i in 1:nrow(aptos_city)) {
    aux_dist <- jarowinkler(tolower(aptos_city$city_name[i]),
                            tolower(mapa_barrio$NOMBBARR))
    if (max(aux_dist) >= 0.85){
      aptos_city[i,2] <- mapa_barrio$NOMBBARR[which.max(aux_dist)]
    }
  }
  
  
  aptos_city %>% filter(NOMBBARR == 'no match')
  aptos_city$city_name2 <- recode(aptos_city$city_name2, 
                                  'Arroyo Seco' = 'Aguada',
                                  'Bella Vista' = 'Reducto',
                                  'Bolivar' = 'Mercado Modelo, Bolivar',
                                  'Colón' = 'Colon Centro y Noroeste',
                                  'Goes' = 'Aguada',
                                  'Montevideo' = 'Centro',
                                  'Parque Batlle'='Pque. Batlle, V. Dolores',
                                  'Paso Molino' = 'Belvedere',
                                  'Perez Castellanos' = 'Castro, P. Castellanos',
                                  'Puerto Buceo' = 'Buceo',
                                  'Villa Dolores'='Pque. Batlle, V. Dolores',
                                  'Punta Rieles'='Pta. Rieles, Bella Italia',
                                  'Villa Biarritz' = 'Punta Carretas')
  
  
  
  aptos_city <- aptos_city %>% #punta rieles lo toma como punta carretas
    mutate(NOMBBARR = ifelse(NOMBBARR == 'no match' | city_name == 'Punta Rieles',
                             city_name2, NOMBBARR)) %>%
    dplyr::select(-city_name2)
  
  datos_entrada <- datos_entrada %>% left_join(aptos_city, by ='city_name')
  
  
  #####
  
  # Agregamos distancia al shopping
  
  #Leemos puntos shoppings
  mall <- st_read("mercado_libre/API/scripts_aux/Mapas/puntos_googlemaps/shoppings")
  #Pasa geometría a formato longlat 
  mall <- st_transform(mall, '+proj=longlat +zone=21 +south +datum=WGS84 +units=m +no_defs')
  mall <- mall %>% select(Name, geometry)
  
  shops <- st_coordinates(mall) %>% data.frame()
  
  colnames(shops) <- c("lon","lat","id")
  
  
  # Obtenemos baricentro por barrio
  
  centroide_barrios <- st_centroid(mapa_barrio)
  centroide_barrios <- centroide_barrios %>%
    mutate(lon_barrio = st_coordinates(centroide_barrios$geometry)[,1],
           lat_barrio = st_coordinates(centroide_barrios$geometry)[,2]) %>%
    select(NOMBBARR, lon_barrio, lat_barrio)
  
  # Agregamos lat y long baricentro por barrios
  
  datos_entrada <- left_join(datos_entrada,centroide_barrios,by="NOMBBARR")
  
  
  datos_entrada$dist_shop <- NA
  
  for(i in 1:nrow(datos_entrada)){
    
    if(!is.na(datos_entrada$lon_barrio[i])){
      
      aux_dist <- distm(c(datos_entrada$lon_barrio[i],datos_entrada$lat_barrio[i]),c(shops$lon[1],shops$lat[1]),fun = distHaversine
      ) 
      
      
      
      for(j in 2:nrow(shops)){
        
        
        aux_dist_2  <- distm(c(datos_entrada$lon_barrio[i],datos_entrada$lat_barrio[i]),c(shops$lon[j],shops$lat[j]),fun = distHaversine
        ) 
        
        
        if(aux_dist_2 < aux_dist){
          
          aux_dist <- aux_dist_2
          
          datos_entrada$dist_shop[i] <- aux_dist[1]
          
        } else {
          
          datos_entrada$dist_shop[i] <- aux_dist[1]
          
          
        }
        
      }    
      
    } 
    
  }
  
  # Recodificacion de la variable dist_shop en niveles
  
  
  
  datos_entrada <- datos_entrada %>% mutate(dist_shop = factor(case_when(dist_shop < 1000 ~ 'Menos de 1 km',
                                                dist_shop < 5000 ~ 'Entre 1 km y 5 km',
                                                TRUE ~ 'Más de 5 km')))
  
  
  
  #### Norte - Sur de Av italia
  
  
  #Leemos linea avd_italia
  avd_italia <- st_read("mercado_libre/API/scripts_aux/Mapas/lineas_googlemaps/avditalia_18")
  #Pasa geometría a formato longlat 
  avd_italia <- st_transform(avd_italia, '+proj=longlat +zone=21 +south +datum=WGS84 +units=m +no_defs')
  avd_italia <- avd_italia %>% select(Name, geometry)
  
  # Extrae latitud y longitud de puntos en avd_italia (geometria)
  puntos_avditalia <- st_coordinates(avd_italia)
  puntos_avditalia <- as_tibble(puntos_avditalia) %>% select(-Z, -L1) %>%
    rename('lon_avditalia' = 'X',
           'lat_avditalia' = 'Y')
  
  
  
  centroide_barrios <- centroide_barrios %>% 
    mutate(aux_lon = NA,
           zona_avditalia = NA)
  
  
  for (i in 1:nrow(centroide_barrios)) {
    centroide_barrios$aux_lon[i] <- which.min(abs(centroide_barrios$lon_barrio[i] - 
                                                    puntos_avditalia$lon_avditalia))
    centroide_barrios$zona_avditalia[i] <- ifelse(
      puntos_avditalia$lat_avditalia[centroide_barrios$aux_lon[i]] < 
        centroide_barrios$lat_barrio[i], 'Norte', 'Sur')
  }
  
  
  
  centroide_barrios <- centroide_barrios %>% 
    data.frame() %>% 
    select(NOMBBARR, zona_avditalia)
  
  
  datos_entrada <- datos_entrada %>% left_join(centroide_barrios, by = 'NOMBBARR')
  
  
  
  
  return(datos_entrada)
  
  
  
  
  
}
