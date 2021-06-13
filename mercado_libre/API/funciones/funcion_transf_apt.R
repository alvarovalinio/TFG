# Script donde se aplica la transformación de datos a los que cargamos mensualmente

# Cargamos funcion para filtrar los numeros con secuencia repetida

# Librerias

library(data.table)
library(tidyverse)
library(magrittr)
library(rvest)
library(here)
library(lubridate)

# Funciones auxiliares

source(here("mercado_libre/API/funciones","funcion_secuencia_3.R"))

source(here("mercado_libre/API/funciones","funcion_zonas_apt.R"))


transf_apt <- function(datos){

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
  
  
  # Agregamos variable segun la informacion observada en mapas.R
  
  # Agregamos variable con nomeclatura datos ine (logica mapa)
  
  aptos <- zonas_barrios(aptos)
  
  zonas <- aptos %>% group_by(NOMBBARR) %>% 
    summarise(mediana = median(price,na.rm = T)) %>% mutate(zona = case_when(
      mediana <= 100000 ~ "Bajo",
      mediana <= 240000 ~ "Medio",
      TRUE ~ "Alto"
    )) 
  
  
  aptos <- full_join(aptos,zonas[,-c(2)],by="NOMBBARR")
  
  # proporcion de NAs
  
  p_na <- sapply(aptos, function(x) round(sum(is.na(x))/length(x),2)) %>% data.frame() %>% 
    rename(prop_na=".") %>% arrange(desc(prop_na))
  
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
                   -buying_mode
                   )
  
 
 return(list(aptos=aptos,id_rep=id_rep,p_na=p_na))
 
 }