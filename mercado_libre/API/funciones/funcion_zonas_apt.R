# Funcion para mapear barrios datos - barrios INE

library(here)
library(data.table)
library(tidyverse)
library(lubridate)
library(sf) # Mapas
library(scales) 
library(RecordLinkage) #funcion para match palabras



zonas_barrios <- function(datos){
  
  
  #Leemos vctorial INE
  mapa_barrio <- st_read("mercado_libre/API/scripts_aux/Mapas/vectorial_INE_barrios/ine_barrios")
  #Pasa geometría a formato longlat 
  mapa_barrio <- st_transform(mapa_barrio, '+proj=longlat +zone=21 +south +datum=WGS84 +units=m +no_defs')
  
  ### match city names - mapa barrio
  aptos_city <- levels(as.factor(datos$city_name))
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
  
  datos <- datos %>% left_join(aptos_city, by ='city_name')
  
  
  
  return(datos)
  
  
  
  
  
}
