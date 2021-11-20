# Funcion para mapear barrios datos - barrios INE

library(here)
library(data.table)
library(tidyverse)
library(lubridate)
library(sf) # Mapas
library(RecordLinkage) #funcion para match palabras
library(gdata)


dist_barrios <- function(datos_entrada){
  
  
  #Leemos vctorial INE
  mapa_barrio <- st_read("Fuentes_Externas/Mapas/INE_barrios")
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
  
  # Agregamos ingreso promedio barrios ECH
  
  load("Fuentes_Externas/ECH/HyP_2020_Terceros.RData")
  
  
  f <- f %>% select(numero, nper, hogar, nombarrio, HT11, ht13, YHOG, YSVL, lp_06, pobre_06,
                    i228, i174, i259, i175, h155, h155_1, h156, h156_1, pesomen) %>%
    filter(hogar == 1)
  
  # Para considerar pesos, multiplicar ingreso hogar i* peso hogar i
  # Sum ingreso hogar i * peso hogar i / sum
  
  f <- f %>% 
    group_by(nombarrio) %>%
    summarise(ingresomedio_ech = sum(pesomen*HT11, na.rm = TRUE) / 
                sum(pesomen, na.rm = TRUE)) %>% 
    rename('NOMBBARR' = 'nombarrio') %>% mutate(NOMBBARR = trim(NOMBBARR))
  
  # Recodificamos los barrios de "f" que no son los mismos que el INE
  
  f$NOMBBARR <- recode(f$NOMBBARR, 
                       'Malvín'='Malvin',
                       'Malvín Norte'='Malvin Norte',
                       'Maroñas, Parque Guaraní'='Maroñas, Parque Guarani',
                       'Unión'='Union')
  
  datos_entrada <- left_join(datos_entrada, f, by = 'NOMBBARR')
  
  
  #### Norte - Sur de Av italia
  
  
  #Leemos linea avd_italia
  avd_italia <- st_read("Fuentes_externas/Mapas/zona_avditalia")
  #Pasa geometría a formato longlat 
  avd_italia <- st_transform(avd_italia, '+proj=longlat +zone=21 +south +datum=WGS84 +units=m +no_defs')
  avd_italia <- avd_italia %>% select(Name, geometry)
  
  # Extrae latitud y longitud de puntos en avd_italia (geometria)
  puntos_avditalia <- st_coordinates(avd_italia)
  puntos_avditalia <- as_tibble(puntos_avditalia) %>% select(-Z, -L1) %>%
    rename('lon_avditalia' = 'X',
           'lat_avditalia' = 'Y')
  
  # Obtenemos baricentro por barrio
  
  centroide_barrios <- st_centroid(mapa_barrio)
  centroide_barrios <- centroide_barrios %>%
    mutate(lon_barrio = st_coordinates(centroide_barrios$geometry)[,1],
           lat_barrio = st_coordinates(centroide_barrios$geometry)[,2]) %>%
    select(NOMBBARR, lon_barrio, lat_barrio)
  
  
  
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
  
  
  
  centroide_barrios_aux <- centroide_barrios %>% 
    data.frame() %>% 
    select(NOMBBARR, zona_avditalia,lon_barrio,lat_barrio)
  
  
  datos_entrada <- datos_entrada %>% left_join(centroide_barrios_aux, by = 'NOMBBARR')
  
  
  ###### Sustituimos los NA de lat - lon por el centroide de su respectivo barrio
  
  
  datos_entrada <- datos_entrada %>% mutate(longitude = ifelse(!is.na(longitude),longitude,
                                                               lon_barrio),
                                            latitude =ifelse(!is.na(latitude),latitude,
                                                             lat_barrio))
  
  
  ## Paso previo : Vemos si la obs esta al norte - sur y luego comparamos con
  # norte - sur del barrior para ver si usamos la lat-lon original o el baricentro
  
  datos_entrada <- datos_entrada %>% 
    mutate(aux_lon = NA,
           zona_avditalia_aux = NA)
  
  
  for (i in 1:nrow(datos_entrada)) {
    datos_entrada$aux_lon[i] <- which.min(abs(datos_entrada$longitude[i] - 
                                                puntos_avditalia$lon_avditalia))
    datos_entrada$zona_avditalia_aux[i] <- ifelse(
      puntos_avditalia$lat_avditalia[datos_entrada$aux_lon[i]] < 
        datos_entrada$latitude[i], 'Norte', 'Sur')
  }
  
  datos_entrada <- datos_entrada %>% mutate(longitude = ifelse(zona_avditalia==zona_avditalia_aux,
                                                               longitude,
                                                               lon_barrio),
                                            latitude = ifelse(zona_avditalia==zona_avditalia_aux,
                                                              latitude,
                                                              lat_barrio)) %>% 
    select(-aux_lon,-zona_avditalia_aux)
  
  #####
  
  # Agregamos distancia al shopping
  
  #Leemos puntos shoppings
  mall <- st_read("Fuentes_Externas/Mapas/centros_comerciales")
  #Pasa geometría a formato longlat 
  mall <- st_transform(mall, '+proj=longlat +zone=21 +south +datum=WGS84 +units=m +no_defs')
  mall <- mall %>% select(Name, geometry)
  
  shops <- st_coordinates(mall) %>% data.frame()
  
  colnames(shops) <- c("lon","lat","id")
  
  
  datos_entrada$dist_shop <- NA
  
  for(i in 1:nrow(datos_entrada)){
    
    if(!is.na(datos_entrada$longitude[i])){
      
      aux_dist <- distm(c(datos_entrada$longitude[i],datos_entrada$latitude[i]),c(shops$lon[1],shops$lat[1]),fun = distHaversine
      ) 
      
      
      
      for(j in 2:nrow(shops)){
        
        
        aux_dist_2  <- distm(c(datos_entrada$longitude[i],datos_entrada$latitude[i]),c(shops$lon[j],shops$lat[j]),fun = distHaversine
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
  
  
  
  # datos_entrada <- datos_entrada %>% mutate(dist_shop = factor(case_when(dist_shop < 1000 ~ 'Menos de 1 km',
  #                                                                        dist_shop < 5000 ~ 'Entre 1 km y 5 km',
  #                                                                        TRUE ~ 'Más de 5 km')))
  # 
  
  
  
  #### Calculamos distancia a la playa
  
  # Rambla Este - MVD
  
  # Cargamos datos
  
  rambla <- st_read("Fuentes_Externas/Mapas/rambla_Este")
  rambla <- st_transform(rambla, '+proj=longlat +zone=21 +south +datum=WGS84 +units=m +no_defs')
  rambla <- rambla %>% select(Name, geometry)
  
  
  rambla_crs <- st_coordinates(rambla) %>% data.frame()
  rambla_crs <- rambla_crs %>% select(-Z, - L1) %>%
    rename('lon' = 'X',
           'lat' = 'Y')
  
  
  datos_entrada$dist_rambla <- NA
  
  for(i in 1:nrow(datos_entrada)){
    
    if(!is.na(datos_entrada$longitude[i]) & !is.na(datos_entrada$latitude[i]) ){
      
      aux_dist <- distm(c(datos_entrada$longitude[i],datos_entrada$latitude[i]),
                        c(rambla_crs$lon[1],rambla_crs$lat[1]),fun = distHaversine
      ) 
      
      
      
      for(j in 2:nrow(rambla_crs)){
        
        
        aux_dist_2  <- distm(c(datos_entrada$longitude[i],datos_entrada$latitude[i]),
                             c(rambla_crs$lon[j],rambla_crs$lat[j]),fun = distHaversine
        ) 
        
        
        if(aux_dist_2 < aux_dist){
          
          aux_dist <- aux_dist_2
          
          datos_entrada$dist_rambla[i] <- aux_dist[1]
          
        } else {
          
          datos_entrada$dist_rambla[i] <- aux_dist[1]
          
          
        }
        
      }    
      
    } 
    
  }
  
  
  
  datos_entrada <- datos_entrada %>% select(-lon_barrio,lat_barrio)
  
  
  return(datos_entrada)
  
  
  
  
  
}