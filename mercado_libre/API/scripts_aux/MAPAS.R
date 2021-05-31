rm(list = ls())

library(readr)
library(here)
library(data.table)
library(tidyverse)
library(lubridate)
library(magrittr)
library(sf) # Mapas
library(leaflet) # Mapas
library(usethis)
library(googlesheets4)
library(rgdal)
library(sp)
library(raster)
library(RColorBrewer)
library(scales) 


barrios_aptos <- list.files(path = here("mercado_libre/API/datos/apt/202105"), pattern = "*.csv", full.names = T)

aptos_total <- sapply(barrios_aptos, FUN=function(id_barrio){
      read_csv(file=id_barrio,col_types = cols(.default = "c"))}, simplify=FALSE) %>% bind_rows()

aptos_total <- aptos_total %>%  
      separate(col = 'covered_area', into = c("covered_area", "covered_area_unidad"), sep = "\\s") %>% 
      separate(col = 'total_area', into = c('total_area', 'total_area_unidad'), sep = "\\s") %>%
      mutate(covered_area =  as.numeric(covered_area),
             total_area = as.numeric(total_area),
             price = as.numeric(price),
             latitude = as.numeric(latitude),
             longitude = as.numeric(longitude),
             dia = as_date(dia),
             date_created = as_date(date_created),
             last_updated = as_date(last_updated))

aptos_total <- as_tibble(aptos_total)
cuantil <- aptos_total %>% 
      filter(price < 40000) %>% 
      dplyr::select(price) %$% 
      quantile(.$price, 0.25)

aptos <- aptos_total %>% filter(price != 11111111, price != 111111111, price > cuantil)

aptos <- aptos %>% filter(latitude < -30, longitude < -50, longitude > -60)
#aptos_f <- aptos[1:10, ]
st_crs(aptos)
#aptos_coord <- st_transform(aptos_coord, '+proj=longlat +zone=21 +south +datum=WGS84 +units=m +no_defs')


mapa_barrio <- st_read("vectoriales_INE/ine_barrios")
#mapa_barrio <- st_transform(mapa_barrio, '+proj=longlat +zone=21 +south +datum=WGS84 +units=m +no_defs')


nro_obs <- as_tibble(aptos) %>% dplyr::select(city_name) %>% 
      group_by(city_name) %>%
      summarise(conteo = n()) %>%
      mutate(prop = conteo/sum(conteo)) %>%
      rename('NOMBBARR' = 'city_name') 

mapa_barrio <- mapa_barrio %>% left_join(nro_obs, by = 'NOMBBARR')


aptos_f <- aptos[1:1000,]

pal <- colorRamp(c("#000000", "#FFFFFF"), interpolate="spline")



mapa_barrio %>% mutate(prop = ifelse(is.na(prop), 0, prop)) %>%
leaflet() %>% 
      addPolygons(fillColor = ~pal(prop), weight = 2, opacity = 0.5, 
                  smoothFactor = 0.5, fillOpacity = 0.7,
                   color = 'black', label=~NOMBBARR,  dashArray = "3",
                  highlight = highlightOptions(weight = 10,
                                               color = "green",
                                               bringToFront = TRUE)) %>%
      addCircles(data = aptos_f, lng = ~longitude, lat = ~latitude, color = 'red') %>%
      addTiles()

#aptos <- st_as_sf(aptos, coords = c("longitude", "latitude"), 
#                 crs = 4326, agr = "constant")

aptos <- aptos %>% mutate(city_name = ifelse(city_name == 'Cordón','Cordon',
                                             ifelse(city_name == 'Ituzaingó','Ituzaingo',
                                                    ifelse(city_name == 'Jardines Hipódromo','Jardines Hipodromo',
                                                           ifelse(city_name == 'Unión', 'Union',
                                                                  ifelse(city_name == 'Parque Rodó', 'Parque Rodo',
                                                                         ifelse(city_name == 'Colón', 'Colon',
                                                                                ifelse(city_name == 'Nuevo París', "Nuevo Paris", city_name))))))))
levels(as.factor(aptos$city_name))

m <- aptos %>% 
      group_by(city_name) %>% 
      summarise(price=mean(price),
                conteo = n()) %>%
      mutate(prop = conteo / sum(conteo)) %>%
      arrange(desc(price))

m <- m %>% rename(NOMBBARR=city_name)

mapa_barrio <- mapa_barrio %>% left_join(m, by="NOMBBARR")
mapa_barrio <- mapa_barrio %>% arrange(desc(price))


#hay que pasar geometría a formato longlat 
mapa_barrio <- st_transform(mapa_barrio, '+proj=longlat +zone=21 +south +datum=WGS84 +units=m +no_defs')


#con geom_sf_text pone solamente el texto del label
ggplot(mapa_barrio)+
      geom_sf(aes(fill=price)) +
      geom_sf_label(data = mapa_barrio[1:30,], aes(label = NOMBBARR), 
                   color = 'black', fill = 'gray', size = 2.5) +
      coord_sf(xlim=c(-56.2,-56.05), ylim = c(-34.94, -34.86)) +
      xlab('Longitud') +
      ylab('Latitud') +
      theme(axis.text.x = element_text(angle = 45))

ggplot(mapa_barrio)+
      geom_sf(aes(fill=price)) +
      geom_sf_text(data = mapa_barrio[1:15,], aes(label = NOMBBARR), 
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
            legend.text = element_text(angle = 90, size = 7),
            legend.title = element_text(size = 9, face = 'bold', hjust = 0.5)) +
      scale_fill_gradient( name = "Precio \n promedio",
                          labels = comma)

#scale_fill_gradient(low = "white", high = "green", name = "Precio \n promedio",
                    
ggplot(mapa_barrio)+
      geom_sf(aes(fill=conteo.x)) +
      geom_sf_text(data = mapa_barrio[1:30,], aes(label = NOMBBARR), 
                   color = 'black', fill = 'gray', size = 3) +
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
            legend.text = element_text(angle = 90, size = 7),
            legend.title = element_text(size = 9, face = 'bold', hjust = 0.5)) +
      scale_fill_gradient( low = 'gray', high = 'darkgreen',name = "Cantidad de \n apartamentos",
                           labels = comma)


#oeste y sur se filtra con signo negativo
#norte positivo

a <- anti_join(mapa_barrio, m, by = 'NOMBBARR')

#mapa_barrio <- st_transform(mapa_barrio, '+proj=longlat +zone=21 +south +datum=WGS84 +units=m +no_defs')
# aptos_coord <- st_transform(aptos_coord, '+proj=longlat +zone=21 +south +datum=WGS84 +units=m +no_defs')
