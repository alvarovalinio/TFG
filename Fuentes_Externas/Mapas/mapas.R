# Script para la creacion de los diferentes mapas utilizados

rm(list = ls())

library(here)
library(tidyverse)
library(sf) # Mapas
library(scales) 

### Datos ====

source(here("Funciones","funcion_imput_media.R"))

aptos_yearmonth <- list.files(path = here("Datos/Limpios"), 
                              pattern = "*.csv", full.names = T)

yearmonth <- c('aptos_202106','aptos_202107',"aptos_202108", "aptos_202109", "aptos_202110" )

aptos <- sapply(aptos_yearmonth, FUN=function(yearmonth){
  read_csv(file=yearmonth)}, simplify=FALSE) %>% bind_rows()

aptos <- aptos %>% group_by(id) %>% 
  arrange(desc(fecha_bajada)) %>%
  slice(1) %>% ungroup()

aptos <- aptos %>% mutate_if(is.character, as.factor)

# Eliminamos obs. con precio superior al percentil 95%

aptos <- aptos %>% filter(price <= quantile(aptos$price,.95))

### Shapefiles ====

# Vectoria INE
mapa_barrio <- st_read(here("Fuentes_externas/Mapas", "INE_barrios"), quiet = TRUE)
mapa_barrio <- st_transform(mapa_barrio, '+proj=longlat +zone=21 +south +datum=WGS84 +units=m +no_defs')

# Geometría centros comerciales
mall <- st_read(here("Fuentes_externas/Mapas","centros_comerciales"), quiet = TRUE)
mall <- st_transform(mall, '+proj=longlat +zone=21 +south +datum=WGS84 +units=m +no_defs')
mall <- mall %>% select(Name, geometry)

# Geometría zona_avditalia
avd_italia <- st_read(here("Fuentes_externas/Mapas","zona_avditalia"), quiet = TRUE)
avd_italia <- st_transform(avd_italia, '+proj=longlat +zone=21 +south +datum=WGS84 +units=m +no_defs')
avd_italia <- avd_italia %>% select(Name, geometry)

# Geometría Rambla Este - MVD
rambla <- st_read(here("Fuentes_externas/Mapas", "rambla_Este"), quiet = TRUE)
rambla <- st_transform(rambla, '+proj=longlat +zone=21 +south +datum=WGS84 +units=m +no_defs')
rambla <- rambla %>% select(Name, geometry)

# Centroide barrios

#Devuleve geometría con el centroide de cada barrios
centroide_barrios <- st_centroid(mapa_barrio)

# Extrae coordenadas (longitud y latitud) de geometría del centroide
centroide_barrios <- centroide_barrios %>%
  mutate(lon_barrio = st_coordinates(centroide_barrios$geometry)[,1],
         lat_barrio = st_coordinates(centroide_barrios$geometry)[,2])

# Pasa latitud y longitud del centroide a objeto sf
centroide_barrios_sf <- centroide_barrios %>% 
  st_as_sf(coords = c("lat_barrio","lon_barrio"), crs='+proj=longlat +zone=21 +south +datum=WGS84 +units=m +no_defs')

# Transforma coordenadas a formato long lat
centroide_barrios_sf_t <- st_transform(centroide_barrios_sf,crs='+proj=longlat +zone=21 +south +datum=WGS84 +units=m +no_defs')

# Auxiliar
centroide_barrios <- centroide_barrios %>% 
  mutate(aux_lon = NA,
         zona_avditalia = NA)

# Extrae latitud y longitud de la geometría zona_avditalia
puntos_avditalia <- st_coordinates(avd_italia)

puntos_avditalia <- as_tibble(puntos_avditalia) %>% select(-Z, -L1) %>%
  rename('lon_avditalia' = 'X',
         'lat_avditalia' = 'Y')

# Geometría avd italia se conforma en total de 60 puntos
# For loop  - asigna zona a cada barrio 

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

mapa_barrio <- mapa_barrio %>% left_join(centroide_barrios, by = 'NOMBBARR')

### Mapas ====

m1 <- ggplot(mapa_barrio) +
  geom_sf(aes(fill = zona_avditalia )) +
  geom_sf(data = mall) +
  geom_sf(data = avd_italia, color = 'blue', size = 0.5) +
  geom_sf(data = rambla, color = 'yellow2', size = 0.5) +
  theme(axis.text.x = element_text(angle = 45),
        text = element_text(),
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
  xlab('Longitud') +
  ylab('Latitud') +
  scale_fill_manual(name = 'Zona Avd. \n Italia', values = c('orangered2', 'springgreen4'))


#### ECH ====

load(here("Fuentes_Externas/ECH/HyP_2020_Terceros.RData"))

f <- f %>% 
  select(numero, nper, hogar, nombarrio, HT11, ht13, YHOG, YSVL, lp_06, pobre_06,
                  i228, i174, i259, i175, h155, h155_1, h156, h156_1, pesomen) %>%
  filter(hogar == 1)

# Considera pesos ECH

f <- f %>% 
  group_by(nombarrio) %>%
  summarise(media_ingbarr = sum(pesomen*HT11, na.rm = TRUE) / 
              sum(pesomen, na.rm = TRUE))

f <- f %>% rename('NOMBBARR' = 'nombarrio')

# Quitamos espacios en blanco al final de nombbarr
f$NOMBBARR <- trimws(f$NOMBBARR, which = "right", whitespace = "[ \t\r\n]")

f$NOMBBARR <- recode(as.factor(f$NOMBBARR), 
                     'Malvín' = 'Malvin',
                     'Malvín Norte' = 'Malvin Norte',
                     'Unión' = 'Union',
                     'Maroñas, Parque Guaraní' = 'Maroñas, Parque Guarani',
                     'Villa García, Manga Rur.' = 'Villa Garcia, Manga Rur.')

mapa_barrio <- mapa_barrio %>% left_join(f, by = 'NOMBBARR')

m2 <- ggplot(mapa_barrio) +
  geom_sf(aes(fill = media_ingbarr/1000 )) +
  theme(axis.text.x = element_text(angle = 45),
        text = element_text(),
        panel.grid.major = element_line(
          color = '#cccccc',linetype = 'dashed',size = .3),
        panel.background = element_rect(fill = 'aliceblue'),
        axis.title = element_blank(),
        axis.text = element_text(size = 8),
        legend.position = 'bottom',
        legend.text = element_text(angle = 0, size = 7),
        legend.title = element_text(size = 9, face = 'bold', hjust = 0.5)) +
  xlab('Longitud') + ylab('Latitud') +
  scale_fill_gradient(low = 'red', high = 'green', name = "Ingreso promedio \n por mil ECH",labels = comma) 
