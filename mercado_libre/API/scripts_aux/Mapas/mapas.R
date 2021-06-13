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

# Funciones auxiliares

source(here("mercado_libre/API/funciones","funcion_transf_apt.R"))


barrios_aptos <- list.files(path = here("mercado_libre/API/datos/apt/202106"), pattern = "*.csv", full.names = T)

datos <- sapply(barrios_aptos, FUN=function(id_barrio){
      read_csv(file=id_barrio,col_types = cols(.default = "c"))}, simplify=FALSE) %>% bind_rows()


aptos <- transf_apt(datos)[["aptos"]]


#filtramos lat y long valores que tienen sentido
#aptos <- aptos %>% filter(latitude < -30, longitude < -50, longitude > -60)

#Leemos vctorial INE
mapa_barrio <- st_read("mercado_libre/API/script_aux/Mapas/vectorial_INE_barrios/ine_barrios")
#Pasa geometría a formato longlat 
mapa_barrio <- st_transform(mapa_barrio, '+proj=longlat +zone=21 +south +datum=WGS84 +units=m +no_defs')

### match city names - mapa barrio
aptos_city <- levels(as.factor(aptos$city_name))
aptos_city <- as.data.table(aptos_city) %>% 
      rename(city_name = aptos_city) %>%
      mutate(NOMBBARR = 'no match') %>%
      mutate(city_name2 = city_name)


for (i in 1:nrow(aptos_city)) {
      aptos_city[i, 2] = ifelse(length(agrep(aptos_city[i,1], mapa_barrio$NOMBBARR, ignore.case = TRUE, value = TRUE)) > 0,
                             agrep(aptos_city[i,1], mapa_barrio$NOMBBARR, ignore.case = TRUE, value = TRUE)[1],
                             aptos_city[i,2])
}


#vemos los que no matchearon y lo hacemos manual
aptos_city %>% filter(NOMBBARR == 'no match')
aptos_city$city_name2 <- recode(aptos_city$city_name2, 'Jardines Hipódromo' = 'Jardines del Hipodromo',
                                    'Maroñas, Curva'= 'Flor de Maroñas',
                                    'Montevideo' = 'Centro',
                                    'Parque Batlle'='Pque. Batlle, V. Dolores',
                                    'Pocitos Nuevo' = 'Pocitos',
                                    'Puerto Buceo' = 'Buceo',
                                    'Punta Rieles'='Pta. Rieles, Bella Italia',
                                    'Perez Castellanos' = 'Castro, P. Castellanos',
                                    'Villa Dolores'='Pque. Batlle, V. Dolores',
                                    'Villa Biarritz' = 'Punta Carretas',
                                    'Arroyo Seco' = 'Reducto')

aptos_city <- aptos_city %>% 
      mutate(NOMBBARR = ifelse(NOMBBARR == 'no match', city_name2, NOMBBARR)) %>%
      dplyr::select(-city_name2)

aptos <- aptos %>% left_join(aptos_city, by ='city_name')


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

#PLOTS

ggplot(mapa_barrio)+
      geom_sf(aes(fill=precio_pormil)) +
      geom_sf_text(data = mapa_barrio, aes(label = NOMBBARR), 
                   color = 'white', fill = 'gray', size = 3) +
      coord_sf(xlim=c(-56.2,-56.05), ylim = c(-34.94, -34.86)) +
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

library(maptools)

distm(x = c(aptos$longitude[1],aptos$latitude[1]),
     y = c(aptos$longitude[2],aptos$latitude[1]),
     fun = distHaversine)

tkml <- getKMLcoordinates(kmlfile="mykml.kml", ignoreAltitude=T)


