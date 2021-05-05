# Script para obtener los datos de la api por barrios

library(here)

# Cargamos id_barrios

load(here("mercado_libre/funciones","id_barrios.Rdata"))

# Cargamos funcion

source(here("mercado_libre/funciones","funcion_api_barrios.R"))


# Credenciales

clientID <- Sys.getenv("Client_ID")
secret <- Sys.getenv("Secret_ID")
code <- 'TG-609329b88517690008ae95df-317926679' # Por cada token hay que generar uno
redict_url <-  'https://www.mercadolibre.com.uy/' # Fijado al crear la "app"


prueba_1 <- datos_barrio(clientid = clientID,
                       secret = secret,
                       code = code,
                       redict_url = redict_url,
                       id_barrio = id_barrios$id_city[1],
                       apt = T,
                       token=T,
                       atributos = T)


prueba_2 <- datos_barrio(clientid = clientID,
                         secret = secret,
                         code = code,
                         redict_url = redict_url,
                         id_barrio = id_barrios$id_city[15],
                         apt = T,
                         token=prueba_1$response,
                         atributos = prueba_1$atributos)

prueba_3 <- datos_barrio(clientid = clientID,
                         secret = secret,
                         code = code,
                         redict_url = redict_url,
                         id_barrio = id_barrios$id_city[49],
                         apt = T,
                         token=prueba_2$response,
                         atributos = prueba_2$atributos)


prueba_4 <- datos_barrio(clientid = clientID,
                         secret = secret,
                         code = code,
                         redict_url = redict_url,
                         id_barrio = id_barrios$id_city[id_barrios$id_city=='TUxVQ1BPQzM5ZGRi'],
                         apt = T,
                         token=prueba_3$response,
                         atributos = prueba_3$atributos)


prueba_5 <- datos_barrio(clientid = clientID,
                         secret = secret,
                         code = code,
                         redict_url = redict_url,
                         id_barrio = id_barrios[3],
                         apt = T,
                         token=prueba_4$response,
                         atributos = prueba_4$atributos)
