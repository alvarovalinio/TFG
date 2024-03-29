# Script para obtener los datos de la api por barrios

# Y luego realizar el proceso de limpieza

library(here)

# Cargamos id_barrios

load(here("Auxiliar","id_barrios.Rdata"))

# Cargamos funcion para obtener los datos de la api

source(here("Funciones","funcion_api_barrios.R"))


# Credenciales

clientID <- Sys.getenv("Client_ID")
secret <- Sys.getenv("Secret_ID")
code <- 'TG-6196c1e8bdf3ec001cefcf7c-219703207' # Por cada token hay que generar uno
redict_url <-  'https://www.mercadolibre.com.uy/' # Fijado al crear la "app"

### Apartamentos

ruta_apt_crudos <- paste0("Datos/Crudos/apt") # Modificar ruta para guardar datos crudos

year_month <- paste0(substring(Sys.Date(),1,4),substring(Sys.Date(),6,7))

dir.create(here(ruta_apt_crudos,year_month))

ruta_apt_fecha_crudos <- paste0(ruta_apt_crudos,"/",year_month)


for(i in 1:nrow(id_barrios)){

  if(i==1){
    
  
   resultados <-  datos_barrio(clientid = clientID,
                 secret = secret,
                 code = code,
                 redict_url = redict_url,
                 id_barrio = id_barrios$id_city[i],
                 apt = T,
                 token=T,
                 atributos = T)
    
   nombre <- paste0("barrio_",id_barrios$id_city[i],"_",year_month)
   
   fwrite(resultados$datos,file=here(ruta_apt_fecha_crudos,paste0(nombre,".csv")))
   
   aux_response <- resultados$response
    
   aux_atributos <- resultados$atributos 
   
  } else {
    
    
  resultados <-  datos_barrio(clientid = clientID,
                                secret = secret,
                                code = code,
                                redict_url = redict_url,
                                id_barrio = id_barrios$id_city[i],
                                apt = T,
                                token=aux_response,
                                atributos = aux_atributos)
  
  nombre <- paste0("barrio_",id_barrios$id_city[i],"_",year_month)
  
  fwrite(resultados$datos,file=here(ruta_apt_fecha_crudos,paste0(nombre,".csv")))  
    
  aux_response <- resultados$response
    
  aux_atributos <- resultados$atributos 
    
    
    
  }
  
}


# Realizamos el proceso de limpieza para aptos

# Cargamos funcion

eval(parse(here("Funciones","funcion_transf_apt.R"), encoding="UTF-8"))

ruta_apt_limpios <- paste0("Datos/Limpios/apt")

year_month <- paste0(substring(Sys.Date(),1,4),substring(Sys.Date(),6,7))

# Aplicamos limpieza la datos crudos del mes


barrios_aptos <- list.files(path = here(ruta_apt_fecha_crudos), pattern = "*.csv", full.names = T)

datos <- sapply(barrios_aptos, FUN=function(id_barrio){
  read_csv(file=id_barrio,col_types = cols(.default = "c"))}, simplify=FALSE) %>% bind_rows()

datos$fecha_bajada <- as.numeric(year_month) # RAROs

aptos <- transf_apt(datos,na=TRUE)[["aptos"]]

# Agregamos fecha_bajada

aptos$fecha_bajada <- year_month

# Guardamos archivo

nombre <- paste0("aptos_",year_month)

fwrite(aptos,file=here(ruta_apt_limpios,paste0(nombre,".csv")))  


### Casas

# ruta_casas <- paste0("Datos/Crudos/casas")
# 
# dir.create(here(ruta_casas,year_month))
# 
# ruta_casas_fecha <- paste0(ruta_casas,"/",year_month)
# 
# for(i in 1:nrow(id_barrios)){
#   
#   if(i==1){
#     
#     
#     resultados <-  datos_barrio(clientid = clientID,
#                                 secret = secret,
#                                 code = code,
#                                 redict_url = redict_url,
#                                 id_barrio = id_barrios$id_city[i],
#                                 apt = F,
#                                 token=T,
#                                 atributos = T)
#     
#     nombre <- paste0("barrio_",id_barrios$id_city[i],"_",year_month)
#     
#     fwrite(resultados$datos,file=here(ruta_casas_fecha,paste0(nombre,".csv")))
#     
#     aux_response <- resultados$response
#     
#     aux_atributos <- resultados$atributos 
#     
#   } else {
#     
#     
#     resultados <-  datos_barrio(clientid = clientID,
#                                 secret = secret,
#                                 code = code,
#                                 redict_url = redict_url,
#                                 id_barrio = id_barrios$id_city[i],
#                                 apt = F,
#                                 token=aux_response,
#                                 atributos = aux_atributos)
#     
#     nombre <- paste0("barrio_",id_barrios$id_city[i],"_",year_month)
#     
#     fwrite(resultados$datos,file=here(ruta_casas_fecha,paste0(nombre,".csv")))  
#     
#     aux_response <- resultados$response
#     
#     aux_atributos <- resultados$atributos 
#     
#     
#     
#   }
#   
# }
# 


