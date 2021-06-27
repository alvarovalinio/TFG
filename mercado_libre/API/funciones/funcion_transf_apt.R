# Script donde se aplica la transformación de datos a los que cargamos mensualmente

# Cargamos funcion para filtrar los numeros con secuencia repetida

# Librerias

library(data.table)
library(tidyverse)
library(magrittr)
library(rvest)
library(here)
library(lubridate)
library(geosphere)

#library(naniar) # Para NAS

# Funciones auxiliares

source(here("mercado_libre/API/funciones","funcion_secuencia_3.R"))



transf_apt <- function(datos,na=FALSE){

  datos <- datos %>% select(-"mlu1472-mtrs",-"mlu1472-mtrstotal")  
  
  datos <-  datos %>%  
    separate(col = 'covered_area', into = c("covered_area", "covered_area_unidad"), sep = "\\s") %>% 
    separate(col = 'total_area', into = c('total_area', 'total_area_unidad'), sep = "\\s") %>%
    separate(col = 'maintenance_fee', into = c("maintenance_fee", "maintenance_fee_unidad"), sep = "\\s") %>%
    mutate(covered_area =  as.numeric(covered_area),
           total_area = as.numeric(total_area),
           price = as.numeric(price),
           latitude = as.numeric(latitude),
           longitude = as.numeric(longitude),
           dia = as_date(dia),
           date_created = as_date(date_created),
           last_updated = as_date(last_updated))
  
  
  # A los que no son dolares los dividimos por el tipo de cambio del momento
  
  # Price lo pasamos a dolares y gastos comunes a pesos
  
  url <- "https://www.ine.gub.uy/"
  
  html <- read_html(url)
  
  tipo_cambio <- html %>% html_nodes("tr:nth-child(1) .alignDerecha+ td") %>% html_text() 
  
  tipo_cambio <-  str_replace(str_remove(tipo_cambio," "),",",".")
  
  
  datos <- datos %>% mutate(price=ifelse(currency_id!="USD",
                                         round(price/as.numeric(tipo_cambio[2]))
                                         ,price))
  
  datos <- datos %>% mutate(maintenance_fee=ifelse(maintenance_fee_unidad!="UYU",
                                                   round(as.numeric(maintenance_fee)*as.numeric(tipo_cambio[2]))
                                                   ,as.numeric(maintenance_fee)))
  
  # ¿Imputamos NA o les asignamos valor 0?
  
  # Filtramos obs con precios "raros" (bajos)
  
  cuantil <- datos %>% filter(price < 40000) %>% select(price) %$% quantile(.$price, 0.75)
  
  aptos <- datos %>% filter(price > cuantil)
  
  # Aplicamos funcion para saber si hay secuencia de numeros repetidos ("dedazos")
  
  aptos$secuencia <- sapply(aptos$price,FUN=secuencia_3)
  
  
  aptos <- aptos %>% filter(secuencia=="No")
  
  # Aplicamos funcion secuencia y truncamos valor
  
  aptos$secuencia_m <- sapply(aptos$maintenance_fee,FUN=secuencia_3)
  
  aptos <- aptos %>% 
        mutate(maintenance_fee = ifelse(secuencia_m =="No", maintenance_fee, NA)) %>%
        select(-secuencia_m)
  
  aptos <- aptos %>% mutate(maintenance_fee = ifelse(maintenance_fee <= 50000, maintenance_fee, NA))
  
  # Reemplazamos por NA valores de long y lat que no tienen sentido
  
  aptos <- aptos %>%
        mutate(longitude = ifelse(between(longitude, -56.5,-56), longitude, NA),
               latitude = ifelse(between(latitude, -35, -34.7), latitude, NA))
  
  # Veamos dias de diferencia entre la ultima actualizacion y la fecha de carga
  
  aptos <- aptos %>% mutate(dif_updated=as.numeric(last_updated-date_created)) 
  
  # Creamos variable factor segun diff_update
  
  aptos <- aptos %>% mutate(f_dif_update=case_when(
    dif_updated==0 ~ "NOW",
    dif_updated<365 ~ "LESS 1 Year",
    TRUE ~ "MORE 1 Year"
  ))
  
  
  # Recodifiquemos variable sellect_contact en la variable inmob
  
  # Filtrar por palabras claves si son inmobiliaria y lo demás como que No
  
  for(i in 1:nrow(aptos)){
    
    aptos$seller_contact[i] <- ifelse(mean(str_detect(tolower(aptos$seller_contact[i]), 
                                                      c('propiedad', 
                                                        'inmob', 
                                                        'bienes',
                                                        "escritorio")))>0,
                                      'inmobiliaria', 'particular')
  }
  
  
  # Obtenamos el año-mes de publicacion
  
  aptos <- aptos %>% mutate(year_month=ifelse(month(date_created)<10,
                                              paste0(year(date_created),"0",month(date_created)),
                                              paste0(year(date_created),month(date_created))))
  
  
  # Todas las variables que arrancan con has_ sustuimos NA por "No"
  
  aptos <- aptos %>% mutate_at(vars(starts_with('has_')),
                               ~if_else(is.na(.), 'No',
                                        as.character(.)))
  
  
  # todo los valores raros los ponemos con NA y despues vemos
  
  # Modificamos la variable bedrooms
  
  aptos <- aptos %>% mutate(bedrooms = ifelse(as.numeric(bedrooms)<=20,bedrooms,NA))
  
  # Recodificamos
  
  aptos <- aptos %>% mutate(bedrooms = factor(case_when(
    as.numeric(bedrooms) == 0 ~ '0',
    as.numeric(bedrooms) == 1 ~ '1',
    as.numeric(bedrooms) == 2 ~ '2',
    as.numeric(bedrooms) == 3 ~ '3',
    TRUE ~ '4 o mas'
  )))
  
  
  # Modificamos la variable full_bathrooms
  
  aptos <- aptos %>% mutate(full_bathrooms = ifelse(as.numeric(full_bathrooms)<=10,
                                                    full_bathrooms,NA))
  
  # Recodificamos
  
  aptos <- aptos %>% mutate(full_bathrooms = factor(case_when(
    as.numeric(full_bathrooms) == 0 | as.numeric(full_bathrooms) == 1  ~ '1',
    TRUE ~ '2 o mas'
  )))
  
  
  # Modificamos la variable covered_area, asignamos la secuencia de 3 como NA
  
  aptos$secuencia_area <- sapply(aptos$covered_area,FUN=secuencia_3)
  
  aptos <- aptos %>% mutate(covered_area=ifelse(secuencia_area=='No',
                                                covered_area,NA)) %>% select(-secuencia_area)
  
  
  # Modificamos la variable total_area, asignamos la secuencia de 3 como NA
  
  aptos$secuencia_area <- sapply(aptos$total_area,FUN=secuencia_3)
  
  aptos <- aptos %>% mutate(total_area=ifelse(secuencia_area=='No',
                                                total_area,NA)) %>% select(-secuencia_area)
  
  # Creamos nueva variable, diferencia entre total - covered
  
  aptos <- aptos %>% mutate(no_covered_area =ifelse(total_area - covered_area<0,NA,
                                                    total_area - covered_area))
  
  
  # Para hacer debuggin
  
  aux <- aptos
  
  # Agregamos distancia a los shops
  
  #source(here("mercado_libre/API/funciones","funcion_shops_apt.R"))
  
  eval(parse(here("mercado_libre/API/funciones","funcion_shops_apt.R"), encoding="UTF-8"))
  
  aptos <- shops_barrios(aptos)
  
  
  # Hay ids repetidos?
  
  id_rep <- aptos %>% group_by(id) %>% summarise(cantidad=n()) %>% filter(cantidad>1)
  
  
  
  aptos <- unique(aptos)
 
  
  # Sacamos variables auxiliares - basura

 aptos <-  aptos %>% select(-NOMBBARR,
                   -dif_updated,
                   -currency_id,
                   -maintenance_fee_unidad,
                   -canonical_url,
                   -secuencia,
                   -dia,
                   -total_area_unidad,
                   -operation,
                   -id_state,
                   -id_city,
                   -category_id,
                   -state_name,
                   -apartments_per_floor,
                   -buying_mode,
                   -parking_lots # Valores muy raros
                   )
  
 
 # proporcion de NAs
 
 p_na <- sapply(aptos, function(x) round(sum(is.na(x))/length(x),3)) %>% data.frame() %>% 
   rename(prop_na=".") %>% arrange(desc(prop_na))
 
 
 # Diferenciamos si incluimos aquellas variables que tienen Obs con NA 
 
 # el parámetro na puede ser una tolerancia de la propoción
 
 if(na==TRUE){
   
   return(list(aptos=aptos,id_rep=id_rep,p_na=p_na))
   
 } else if(na==FALSE){
   
   faltan <- c(row.names(p_na %>% filter(prop_na!=0)))
   
   aptos <- aptos %>% select(-c(faltan))
   
   return(list(aptos=aptos,id_rep=id_rep,p_na=p_na))
   
 } else{
   
   
  faltan <- c(row.names(p_na %>% filter(prop_na>na)))
   
   aptos <- aptos %>% select(-c(faltan))
   
   return(list(aptos=aptos,id_rep=id_rep,p_na=p_na))
   
   }
 

 
 }