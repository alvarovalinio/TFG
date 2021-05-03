# Script para obtener los datos de la api por barrios

library(here)

# Cargamos id_barrios

load(here("mercado_libre/funciones","id_barrios.Rdata"))

# Cargamos funcion

source(here("mercado_libre/funciones","funcion_api_barrios.R"))


# Credenciales

clientID <- Sys.getenv("Client_ID")
secret <- Sys.getenv("Secret_ID")
code <- 'TG-608f474fd06d340007e4b921-219703207' # Por cada token hay que generar uno
redict_url <-  'https://www.mercadolibre.com.uy/' # Fijado al crear la "app"


prueba_1 <- datos_barrio(clientid = clientID,
                       secret = secret,
                       code = code,
                       redict_url = redict_url,
                       id_barrio = id_barrios[1],
                       apt = T,
                       token=T)


prueba_2 <- datos_barrio(clientid = clientID,
                         secret = secret,
                         code = code,
                         redict_url = redict_url,
                         id_barrio = id_barrios[15],
                         apt = T,
                         token=prueba_1$response)

prueba_3 <- datos_barrio(clientid = clientID,
                         secret = secret,
                         code = code,
                         redict_url = redict_url,
                         id_barrio = id_barrios[100],
                         apt = T,
                         token=prueba_2$response)


prueba_4 <- datos_barrio(clientid = clientID,
                         secret = secret,
                         code = code,
                         redict_url = redict_url,
                         id_barrio = id_barrios[2],
                         apt = T,
                         token=prueba_3$response)


prueba_5 <- datos_barrio(clientid = clientID,
                         secret = secret,
                         code = code,
                         redict_url = redict_url,
                         id_barrio = id_barrios[3],
                         apt = T,
                         token=prueba_4$response)
