rm(list = ls())

library(here)
library(data.table)
library(tidyverse)
library(lubridate)
library(sf) # Mapas
library(leaflet) # Mapas
library(usethis)
library(sp)
library(scales) 
library(geosphere) #para medir distancias
library(RecordLinkage) #funcion para march palabras

# Funciones auxiliares
source(here("mercado_libre/API/funciones","funcion_transf_apt.R"))

barrios_aptos <- list.files(path = here("mercado_libre/API/datos/apt/202106"), pattern = "*.csv", full.names = T)

datos <- sapply(barrios_aptos, FUN=function(id_barrio){
      read_csv(file=id_barrio,col_types = cols(.default = "c"))}, simplify=FALSE) %>% bind_rows()


#aptos <- transf_apt(datos, na = 0.02)[["aptos"]]
aptos <- datos

######################
##### SHAPEFILES #####
######################

#Leemos los shapefiles y pasamos a formato longlat

# Vectoria INE

mapa_barrio <- st_read("mercado_libre/API/scripts_aux/Mapas/vectorial_INE_barrios/ine_barrios")
mapa_barrio <- st_transform(mapa_barrio, '+proj=longlat +zone=21 +south +datum=WGS84 +units=m +no_defs')

#Puntos shoppings

mall <- st_read("mercado_libre/API/scripts_aux/Mapas/puntos_googlemaps/shoppings")
mall <- st_transform(mall, '+proj=longlat +zone=21 +south +datum=WGS84 +units=m +no_defs')
mall <- mall %>% select(Name, geometry)

# Líneas avd_italia

avd_italia <- st_read("mercado_libre/API/scripts_aux/Mapas/lineas_googlemaps/avditalia_18")
avd_italia <- st_transform(avd_italia, '+proj=longlat +zone=21 +south +datum=WGS84 +units=m +no_defs')
avd_italia <- avd_italia %>% select(Name, geometry)

# Lon y lat aptos - lo transformamos a geometría

aptos <- aptos %>% filter(!is.na(latitude), !is.na(longitude)) %>%
      st_as_sf(coords = c("longitude","latitude"), crs='+proj=longlat +zone=21 +south +datum=WGS84 +units=m +no_defs')

aptos <- st_transform(aptos, crs='+proj=longlat +zone=21 +south +datum=WGS84 +units=m +no_defs')


############################
#### LONGITUD Y LATIDUD ####
############################

# Centroide barrios

#devuleve geometría con el centroide de cada barrios
centroide_barrios <- st_centroid(mapa_barrio)

# Extrae coordenadas (longitud y latitud) degeometría del centroide
centroide_barrios <- centroide_barrios %>%
      mutate(lon_barrio = st_coordinates(centroide_barrios$geometry)[,1],
             lat_barrio = st_coordinates(centroide_barrios$geometry)[,2])

# Pasa latitud y longitud del centroide a objeto sf
centroide_barrios_sf <- centroide_barrios %>% 
      st_as_sf(coords = c("lat_barrio","lon_barrio"), crs='+proj=longlat +zone=21 +south +datum=WGS84 +units=m +no_defs')

# Tranforma coordenadas a formato long lat
centroide_barrios_sf_t <- st_transform(centroide_barrios_sf,crs='+proj=longlat +zone=21 +south +datum=WGS84 +units=m +no_defs')


# Linaeas debajo se usaban para calcuar baricentro como pto medio entre latitudes y longitudes del barrio
# en deshuso ya que usamos función st_centroid

# barrios_coord <- lapply(mapa_barrio$NOMBBARR[1], FUN = function(barrio){
#   aux <- mapa_barrio %>% filter(NOMBBARR == barrio)
#   coordenadas <- st_coordinates(aux$geometry) %>% data.frame()
#   coordenadas$NOMBBARR <- barrio
#   return(coordenadas)
# })[[1]]
# 
# for(i in 2:nrow(mapa_barrio)){
#    aux_barrio_coord <- lapply(mapa_barrio$NOMBBARR[i], FUN = function(barrio){
#     aux <- mapa_barrio %>% filter(NOMBBARR == barrio)
#     coordenadas <- st_coordinates(aux$geometry) %>% data.frame()
#     coordenadas$NOMBBARR <- barrio
#     return(coordenadas)
#   })[[1]]
#   barrios_coord <- rbind(barrios_coord,aux_barrio_coord)
# }

# Extrae latitud y longitud de puntos en avd_italia (geometria)

puntos_avditalia <- st_coordinates(avd_italia)

puntos_avditalia <- as_tibble(puntos_avditalia) %>% select(-Z, -L1) %>%
      rename('lon_avditalia' = 'X',
             'lat_avditalia' = 'Y')

###########################################################
##### MATCH aptos$city_name con vactorial INE NOMBARR #####
###########################################################

# Nombres barrios base aptos != nombres en vectorial INE
# Hacemos march para poder gráficarlos

# NOMBARR a priori vale no match
# city_name2 aux toma valor a city_name
aptos_city <- levels(as.factor(aptos$city_name))
aptos_city <- as.data.table(aptos_city) %>% 
      rename(city_name = aptos_city) %>%
      mutate(NOMBBARR = 'no match') %>%
      mutate(city_name2 = city_name)


# algoritmo para medir distancias entre dos strings
# a cada nivel de city_name en la base de aptos lo compara con cada uno en vectorial INE
# nos quedamos con el que tiene distancia menor

for(i in 1:nrow(aptos_city)) {
      aux_dist <- jarowinkler(tolower(aptos_city$city_name[i]),
                              tolower(mapa_barrio$NOMBBARR))
      if (max(aux_dist) >= 0.85){
            aptos_city[i,2] <- mapa_barrio$NOMBBARR[which.max(aux_dist)]
      }
}

#vemos los que no matchearon (aux_dist < 0.85) y lo hacemos manual
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

aptos_city <- aptos_city %>% #punta rieles lo toma como punta carretas - corregimos
      mutate(NOMBBARR = ifelse(NOMBBARR == 'no match' | city_name == 'Punta Rieles',
                               city_name2, NOMBBARR)) %>%
      dplyr::select(-city_name2)


# A base aptos le pegamos NOMBBARR (nombre barrios en vectorial INE)
aptos <- aptos %>% left_join(aptos_city, by ='city_name')
aptos$city_name <- recode(aptos$city_name, 'Colón' = 'Colon Centro y Noroeste')


#################################
#### NORTE - SUR AVD. ITALIA ####
#################################

# Define puntos aptos al oeste o sur de avd italia 
# Se define segun centroide barrio esté al norte o al sur
# no usamos lat y lon de cada apto ya que hay datos erróneos

# Extrae coordenadas aptos

aptos <- aptos %>% mutate(longitude = st_coordinates(geometry)[,1],
                          latitude = st_coordinates(geometry)[,2])

# Filtra valores lon y lat que no tienen sentido y NAs

aptos <- aptos %>%
      filter(!is.na(latitude), !is.na(longitude)) %>% 
      filter(between(longitude, -56.5,-56)) %>%
      filter(between(latitude, -35, -34.7))

# crea columna auxiliar aux_lon = NA y zona

centroide_barrios <- centroide_barrios %>% 
      mutate(aux_lon = NA,
             zona_avditalia = NA)

# Avd italia se conforma en total de 60 puntos
# Para cada barrios tomamos el punto en avd.italia con menor diferencia de longitud
# min {longitud centroide - longitud avd_italia }
# luego comparamos las latitudes del centroide y el punto de avd italia con mínima diferencia en cuanto a longitud
# si latitud avd italia < lat centroide barrio -> NORTE 
# si latitud avd italia >= lat centroide barrio -> NORTE 

for (i in 1:nrow(centroide_barrios)) {
      centroide_barrios$aux_lon[i] <- which.min(abs(centroide_barrios$lon_barrio[i] - 
                                                          puntos_avditalia$lon_avditalia))
      centroide_barrios$zona_avditalia[i] <- ifelse(
            puntos_avditalia$lat_avditalia[centroide_barrios$aux_lon[i]] < 
                  centroide_barrios$lat_barrio[i], 'Norte', 'Sur')
}


# Pegamos zona_avditalia en base aptos

centroide_barrios <- centroide_barrios %>% 
      data.frame() %>% 
      select(NOMBBARR, zona_avditalia)

aptos <- aptos %>% left_join(centroide_barrios, by = 'NOMBBARR') 



#######################
#### VISUALIZACIÓN ####
#######################

# Visualización centroide barrios y vectorial iNE
ggplot(mapa_barrio)+
  geom_sf(aes(fill=NOMBBARR)) +
  xlab('Longitud') +
  ylab('Latitud') +
  geom_sf_text(data = mapa_barrio, aes(label = NOMBBARR), 
               color = 'white', fill = 'gray', size = 3) +
  theme(axis.text.x = element_text(angle = 45),
        text = element_text(family = 'Avenir'),
        panel.grid.major = element_line(
          color = '#cccccc',linetype = 'dashed',size = .3),
        panel.background = element_rect(fill = 'aliceblue'),
        axis.title = element_blank(),
        axis.text = element_text(size = 8),
        legend.position = 'None',
        legend.text = element_text(angle = 0, size = 7),
        legend.title = element_text(size = 9, face = 'bold', hjust = 0.5)) +
  geom_sf(data=centroide_barrios_sf_t,color = 'black') 



# Mapa temático según median(price)

# hay que pasar aptoa a formato data.table sino se joden las cuentas
m2 <- aptos %>% data.table() %>% 
      group_by(NOMBBARR) %>% 
      summarise(price=median(as.numeric(price), na.rm=TRUE)) %>%
      arrange(desc(price)) %>% 
      mutate(precio_pormil = round(price/1000,0))

mapa_barrio <- mapa_barrio %>%
      left_join(m2, by="NOMBBARR") 

ggplot(mapa_barrio)+
      geom_sf(aes(fill=precio_pormil)) +
      geom_sf_text(data = mapa_barrio, aes(label = NOMBBARR), 
                   color = 'white', fill = 'gray', size = 3) +
      coord_sf(xlim=c(-56.3,-56.04), ylim = c(-34.94, -34.80)) +
      xlab('Longitud') +
      ylab('Latitud') +
      theme(axis.text.x = element_text(angle = 45),
            text = element_text(family = 'Avenir'),
            panel.grid.major = element_line(
                  color = '#cccccc',linetype = 'dashed',size = .3),
            panel.background = element_rect(fill = 'aliceblue'),
            axis.title = element_blank(),
            axis.text = element_text(size = 8),
            legend.position = 'bottom',
            legend.text = element_text(angle = 0, size = 7),
            legend.title = element_text(size = 9, face = 'bold', hjust = 0.5)) +
      scale_fill_gradient(low = 'firebrick', high = 'darkgreen', name = "Mediana \n por mil",
                          labels = comma)

# Mapa completo - puntos shoppings y avd italia

ggplot(mapa_barrio)+
      geom_sf(aes(fill=precio_pormil)) +
      geom_sf(data = mall) +
      geom_sf(data = avd_italia, color = 'yellow', size = 0.7) +
      theme(axis.text.x = element_text(angle = 45),
            text = element_text(family = 'Avenir'),
            panel.grid.major = element_line(
                  color = '#cccccc',linetype = 'dashed',size = .3),
            panel.background = element_rect(fill = 'aliceblue'),
            axis.title = element_blank(),
            axis.text = element_text(size = 8),
            legend.position = 'bottom',
            legend.text = element_text(angle = 0, size = 7),
            legend.title = element_text(size = 9, face = 'bold', hjust = 0.5)) +
      scale_fill_gradient(low = 'firebrick', high = 'darkgreen', name = "Mediana \n por mil",
                          labels = comma) +
      ggrepel::geom_label_repel(data = mall,aes(label = Name, geometry = geometry),
      stat = "sf_coordinates", min.segment.length = 0,
      colour = "black", segment.colour = "black",
      size = 3, alpha = 0.8) +
      xlab('Longitud') +
      ylab('Latitud') 
      

      
# Mapa de puntos zona_avd italia asignada a cada apto
# segun criterio seleccionado centroide del barrio al que pertenece
# los puntos azules que se encuentran al norte son puntos con barrio al sur
# pero que las coordenadas estaban al norte 
# Supuesto: barrio correcto, georreferencia incorrecta

ggplot(mapa_barrio)+
      geom_sf() +
      geom_sf(data = mall) +
      geom_sf(data = avd_italia, color = 'yellow', size = 1) +
      geom_sf(data = aptos, aes(color = zona_avditalia))+
      theme(axis.text.x = element_text(angle = 45),
            text = element_text(family = 'Avenir'),
            panel.grid.major = element_line(
                  color = '#cccccc',linetype = 'dashed',size = .3),
            panel.background = element_rect(fill = 'aliceblue'),
            axis.title = element_blank(),
            axis.text = element_text(size = 8),
            legend.position = 'bottom',
            legend.text = element_text(angle = 0, size = 7),
            legend.title = element_text(size = 9, face = 'bold', hjust = 0.5)) +
      ggrepel::geom_label_repel(data = mall,aes(label = Name, geometry = geometry),
                                stat = "sf_coordinates", min.segment.length = 0,
                                colour = "black", segment.colour = "black",
                                size = 3, alpha = 0.8) +
      xlab('Longitud') + ylab('Latitud')



# Mapa temático segun zona_avditalia
# Barrios cortados por avd italia se asignan al norte o sur según ubicacion centroide

mapa_barrio <- mapa_barrio %>% left_join(centroide_barrios, by = 'NOMBBARR') 
      
ggplot(mapa_barrio)+
            geom_sf(aes(fill = zona_avditalia )) +
      geom_sf(data = mall) +
      geom_sf(data = avd_italia, color = 'yellow', size = 1) +
      theme(axis.text.x = element_text(angle = 45),
            text = element_text(family = 'Avenir'),
            panel.grid.major = element_line(
                  color = '#cccccc',linetype = 'dashed',size = .3),
            panel.background = element_rect(fill = 'aliceblue'),
            axis.title = element_blank(),
            axis.text = element_text(size = 8),
            legend.position = 'bottom',
            legend.text = element_text(angle = 0, size = 7),
            legend.title = element_text(size = 9, face = 'bold', hjust = 0.5)) +
      ggrepel::geom_label_repel(data = mall,aes(label = Name, geometry = geometry),
                                stat = "sf_coordinates", min.segment.length = 0,
                                colour = "black", segment.colour = "black",
                                size = 3, alpha = 0.8) +
      xlab('Longitud') + ylab('Latitud')

