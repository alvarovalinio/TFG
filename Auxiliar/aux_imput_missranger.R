# SCRIPT AUXILIAR PARA IMPUTAR DATOS FALTANTES CON PROP < 0.15 POR MISS RANGER

library(tidyverse)
library(here)
library(ranger)
library(missRanger)
library(data.table)

# Funciones auxiliares

source(here("Funciones","funcion_imput_media.R"))


#### DATOS

aptos_yearmonth <- list.files(path = here("Datos/Limpios/apt"), 
                              pattern = "*.csv", full.names = T)

yearmonth <- c("aptos_202106","aptos_202107","aptos_202108","aptos_202109","aptos_202110")

aptos <- sapply(aptos_yearmonth, FUN=function(yearmonth){
      read_csv(file=yearmonth)}, simplify=FALSE) %>% bind_rows()


aptos <- aptos %>% group_by(id) %>% 
      arrange(desc(fecha_bajada)) %>%
      slice(1) %>% ungroup()

aptos <- aptos %>% mutate_if(is.character, as.factor)

# Filtramos por el criterio en price - eliminamos obs. con price superior al percentil 95%

aptos_todos <- aptos

aptos <- aptos %>% filter(price <= quantile(aptos$price,.95))

# Perdemos esta cantidad de registros

nrow(aptos_todos) - nrow(aptos)

# vemos prop de NA
p_na <- sapply(aptos, function(x) round(sum(is.na(x))/length(x),4)) %>% data.frame() %>% 
      rename(prop_na=".") %>% arrange(desc(prop_na))

#### Definimos variables Sin na imputamos por la media

aptos_sin_na <- imput_media(aptos,p=.1)

### Imputamos las variables anteriores y item_condition por miss ranger

aptos_mr <- missRanger(aptos %>% select(names(aptos_sin_na),item_condition,-price), 
                       pmm.k = 3, num.trees = 500,seed=1234)

aptos_mr$price <- aptos_sin_na$price

## Guardamos los datos

fwrite(aptos_mr,file='aptos_mr.csv')

############################################




