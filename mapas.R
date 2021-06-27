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


aptos <- transf_apt(datos,na=0.02)[["aptos"]]

#Leemos vctorial INE
mapa_barrio <- st_read("mercado_libre/API/scripts_aux/Mapas/vectorial_INE_barrios/ine_barrios")

#Pasa geometría a formato longlat 
mapa_barrio <- st_transform(mapa_barrio, '+proj=longlat +zone=21 +south +datum=WGS84 +units=m +no_defs')

#Leemos puntos shoppings
mall <- st_read("mercado_libre/API/scripts_aux/Mapas/puntos_googlemaps/shoppings")
#Pasa geometría a formato longlat 
mall <- st_transform(mall, '+proj=longlat +zone=21 +south +datum=WGS84 +units=m +no_defs')
mall <- mall %>% select(Name, geometry)

#Leemos linea avd_italia
avd_italia <- st_read("mercado_libre/API/scripts_aux/Mapas/lineas_googlemaps/avd_italia")
#Pasa geometría a formato longlat 
avd_italia <- st_transform(avd_italia, '+proj=longlat +zone=21 +south +datum=WGS84 +units=m +no_defs')
avd_italia <- avd_italia %>% select(Name, geometry)


# Obtenemos las coordenadas por barrio

barrios_coord <- lapply(mapa_barrio$NOMBBARR[1], FUN = function(barrio){
  aux <- mapa_barrio %>% filter(NOMBBARR == barrio)
  coordenadas <- st_coordinates(aux$geometry) %>% data.frame()
  coordenadas$NOMBBARR <- barrio
  return(coordenadas)
})[[1]]

for(i in 2:nrow(mapa_barrio)){
  
  
  aux_barrio_coord <- lapply(mapa_barrio$NOMBBARR[i], FUN = function(barrio){
    aux <- mapa_barrio %>% filter(NOMBBARR == barrio)
    coordenadas <- st_coordinates(aux$geometry) %>% data.frame()
    coordenadas$NOMBBARR <- barrio
    return(coordenadas)
  })[[1]]
  
  barrios_coord <- rbind(barrios_coord,aux_barrio_coord)
  
}

# Obtenemos baricentro por barrio

baricentro_barrios <- barrios_coord %>%
  group_by(NOMBBARR) %>% 
  summarise(lat_barrio = mean(X, na.rm = TRUE),
            lon_barrio = mean(Y, na.rm = TRUE))



# Match city names base aptos - NOMBARR base mapa_barrio
aptos_city <- levels(as.factor(aptos$city_name))
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

#vemos los que no matchearon y lo hacemos manual
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

aptos <- aptos %>% left_join(aptos_city, by ='city_name')

#MAPA SEGUN MEDIA

m <- aptos %>% 
      group_by(NOMBBARR) %>% 
      summarise(price=mean(as.numeric(price), na.rm=TRUE),
                conteo = n()) %>%
      mutate(prop = conteo / sum(conteo)) %>%
      arrange(desc(price))

mapa_barrio <- mapa_barrio %>% 
      left_join(m, by="NOMBBARR") %>% 
      arrange(desc(price)) %>% 
      mutate(precio_pormil = round(price/1000,0))

#Plots

ggplot(mapa_barrio)+
      geom_sf(aes(fill=precio_pormil)) +
      geom_sf_text(data = mapa_barrio, aes(label = NOMBBARR), 
                   color = 'white', fill = 'gray', size = 3) +
      coord_sf(xlim=c(-56.2,-56.04), ylim = c(-34.94, -34.85)) +
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
      scale_fill_gradient(low = 'firebrick', high = 'darkgreen', name = "Precio promedio \n por mil",
                          labels = comma)


ggplot(mapa_barrio)+
      geom_sf(aes(fill=precio_pormil)) +
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
            legend.position = 'bottom',
            legend.text = element_text(angle = 0, size = 7),
            legend.title = element_text(size = 9, face = 'bold', hjust = 0.5)) +
      scale_fill_gradient(low = 'firebrick', high = 'darkgreen', name = "Precio promedio \n por mil",
                          labels = comma)



# Veamos puntos del baricentro en los barrios


#mapa_barrio <- full_join(mapa_barrio, baricentro_barrios, by = 'NOMBBARR')

baricentro_barrios_sf<- baricentro_barrios %>% 
  st_as_sf(coords = c("lat_barrio","lon_barrio"), crs='+proj=longlat +zone=21 +south +datum=WGS84 +units=m +no_defs')

baricentro_barrios_sf_t <- st_transform(baricentro_barrios_sf,crs='+proj=longlat +zone=21 +south +datum=WGS84 +units=m +no_defs')


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
  geom_sf(data=baricentro_barrios_sf_t,aes(color=NOMBBARR))




#MAPA SEGUN MEDIANA

m2 <- aptos %>% 
      group_by(NOMBBARR) %>% 
      summarise(price=median(as.numeric(price), na.rm=TRUE)) %>%
      arrange(desc(price))

mapa_barrio <- mapa_barrio %>% select(-price) %>%
      left_join(m2, by="NOMBBARR") %>% 
      arrange(desc(price)) %>% 
      mutate(precio_pormil = round(price/1000,0))

#Plots

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


ggplot(mapa_barrio)+
      geom_sf(aes(fill=precio_pormil)) +
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
            legend.position = 'bottom',
            legend.text = element_text(angle = 0, size = 7),
            legend.title = element_text(size = 9, face = 'bold', hjust = 0.5)) +
      scale_fill_gradient(low = 'firebrick', high = 'darkgreen', name = "Mediana \n por mil",
                          labels = comma)





## Veamos la distribución de los NA de algunas variables por barrio

p_na <- transf_apt(datos)[["p_na"]]

# Item_condition

m3_na <- aptos %>% filter(is.na(item_condition)) %>%  
  group_by(NOMBBARR) %>% 
  summarise(cantidad_na=n()) %>%
  arrange(desc(cantidad_na))


m3 <-  aptos %>%   
  group_by(NOMBBARR) %>% 
  summarise(cantidad=n()) %>%
  arrange(desc(cantidad)) %>% left_join (m3_na,by="NOMBBARR") %>% 
  mutate(cantidad_na=replace_na(cantidad_na,0)) %>% 
  mutate(percent_na=round(cantidad_na/cantidad,3)) %>% left_join(m2,by="NOMBBARR") 



mapa_barrio <- mapa_barrio %>% select(-price) %>%
  left_join(m3, by="NOMBBARR") %>% 
  arrange(desc(price)) %>% 
  mutate(precio_pormil = round(price/1000,0))

#Plots

library(plotly)

a <- ggplotly(mapa_barrio %>%  ggplot(aes(fill=percent_na,label2=precio_pormil,label3=NOMBBARR))+
  geom_sf(tooltip=c("label2","label3")) +
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
  scale_fill_gradient(low = 'firebrick', high = 'darkgreen', name = "Percent Na \n Item condition",
                      labels = comma))


#Otro formato para etiquetas:
#geom_sf_label(data = mapa_barrio[1:30,], aes(label = NOMBBARR), 
#              color = 'black', fill = 'gray', size = 2.5)



#LEAFLET
#mapa_barrio %>% mutate(prop = ifelse(is.na(prop), 0, prop)) %>%
#leaflet() %>% 
#      addPolygons(fillColor = ~pal(prop), weight = 2, opacity = 0.5, 
#                  smoothFactor = 0.5, fillOpacity = 0.7,
#                   color = 'black', label=~NOMBBARR,  dashArray = "3",
#                  highlight = highlightOptions(weight = 10,
#                                               color = "green",
#                                               bringToFront = TRUE)) %>%
#      addCircles(data = aptos_f, lng = ~longitude, lat = ~latitude, color = 'red') %>%
#      addTiles()

####################
#### DISTANCIAS ####
####################

#distm(c(lon1, lat1), c(lon2, lat2), fun = distHaversine)

#library(maptools)

#distm(x = c(aptos$longitude[1],aptos$latitude[1]),
 #    y = c(aptos$longitude[2],aptos$latitude[1]),
  #   fun = distHaversine)

#tkml <- getKMLcoordinates(kmlfile="mykml.kml", ignoreAltitude=T)


