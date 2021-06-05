# Script donde se aplica la transformación de datos a los que cargamos mensualmente

# Cargamos funcion para filtrar los numeros con secuencia repetida

source(here("mercado_libre/API/funciones","funcion_secuencia_3.R"))


transf_apt <- function(datos){

datos <- datos %>% select(-"mlu1472-mtrs",-"mlu1472-mtrstotal")  
    
 datos <-  datos %>%  
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
  
  
 # Filtramos obs con precios "raros"
 
 cuantil <- datos %>% filter(price < 40000) %>% select(price) %$% quantile(.$price, 0.25)
 
 #aptos <- datos %>% filter(price != 11111111, price != 111111111, price > cuantil)

 aptos <- datos %>% filter(price > cuantil)
 
 
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
 
 aptos <- aptos %>% mutate(inmob=ifelse(is.na(seller_contact),"NO","SI"))
 
 # Obtenamos el año-mes de publicacion
 
 aptos <- aptos %>% mutate(year_month=ifelse(month(date_created)<10,
                                    paste0(year(date_created),"0",month(date_created)),
                                    paste0(year(date_created),month(date_created))))
 
 # Aplicamos funcion para saber si hay secuencia
 
 aptos$secuencia <- sapply(aptos$price,FUN=secuencia_3)
 
 
 aptos <- aptos %>% filter(secuencia=="No")
 
 # Hay ids repetidos?
 
 id_rep <- aptos %>% group_by(id) %>% summarise(cantidad=n()) %>% filter(cantidad>1)
 
 id_rep
 
 aptos <- unique(aptos)
 
 return(aptos)
 
 }