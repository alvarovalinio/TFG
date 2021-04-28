# Script para crear la base datos

library(httr)
library(jsonlite)
library(tidyverse)
library(data.table)

# Credenciales

clientID <- Sys.getenv("Client_ID")
secret <- Sys.getenv("Secret_ID")
code <- 'TG-60846902bbf574000790d496-219703207' # Por cada token hay que generar uno
# Pensar como automatizar el code
redict_url <-  'https://www.mercadolibre.com.uy/' # Fijado al crear la "app"

# Proceso de obtencion de token

response <-  POST(
  'https://api.mercadolibre.com/oauth/token',
  accept_json(),
  authenticate(clientID, secret),
  body = list(client_id=clientID,client_secret=secret,
              grant_type = 'authorization_code',code=code,redirect_uri=redict_url),
  encode = 'form',
  verbose()
)


# Obtenemos el token

mytoken <-  content(response)$access_token

## Guardamos "the access token", tenemos que pasarselo en cada API request
HeaderValue <-  paste0('Bearer ', mytoken)


### Proceso de obtencion de datos

# Difenciamos si se trata de apartamentos o casas

url_apt <- 'https://api.mercadolibre.com/sites/MLU/search?category=MLU1472&limit=50&offset='

url_casas <- 'https://api.mercadolibre.com/sites/MLU/search?category=MLU1466&limit=50&offset='

#Ambos (para atributos)
url_ini <- 'https://api.mercadolibre.com/sites/MLU/search?category=MLU1459&limit=50&offset='


# Obtenemos lista de los atributos (2000 primeras observaciones)

sequence <- seq(from=0, to=5000, by=50)

# Atributos (más variables)

atributos_v <- vector(mode = 'character', length = 0)

for (s in sequence) {
  request_pag <- GET(url = paste0(url_ini, s), add_headers(Authorization=HeaderValue))
  result_pag <- content(request_pag,type="text",encoding = "UTF-8") %>% fromJSON()
  # este for loop devuelve los atributos dentro de las 50 ibs iniciales
  # hay que adaptarlo para chequeat en 50 mas
  for (i in 1:50) {
    for (j in 1:length( result_pag$results$attributes[i][[1]]$id))
      if (!(result_pag$results$attributes[i][[1]]$id[j] %in% atributos_v) == 'TRUE') {
        atributos_v <- c(atributos_v, result_pag$results$attributes[i][[1]]$id[j])
      }
    
    
  }
}

atributos_v <- tolower(atributos_v)

# 1) Apartamentos

total_apt <- fromJSON('https://api.mercadolibre.com/sites/MLU/search?category=MLU1472')$paging$total

sequence_apt<-seq(from=0, to=total_apt, by=50) #el max sale del link

# to = "maximo valor" que deja

inicio <- Sys.time()

# Objeto para ir almacenado los datos

datos_apt <- matrix(ncol = length(atributos_v)+12)
colnames(datos_apt) <- c("id","title","price","currency_id","buying_mode",
                     "condition","accepts_mercadopago",
                     "state_name","city_name","category_id",
                     "latitude","longitude",tolower(atributos_v))
datos_apt <- as_tibble(datos_apt)



# Modificar la URL agregando ID por Casas y Apt (ver sub_categ)

for (j in sequence_apt){
  
  # URL para moverse de a 50
  url<-paste0(url_apt,j)
  
  # Obtenemos los 50 de esa pagina
  request_pag<-GET(url=url,
                   add_headers(Authorization=HeaderValue))
  
  # valores de esa pagina
  
  result_pag <- content(request_pag,type="text",encoding = "UTF-8") %>% fromJSON()
  
  # Extraemos variables
  
  id <- as.data.frame(result_pag$results$id)
  title <- as.data.frame(result_pag$results$title)
  price <- as.data.frame(result_pag$results$price)
  currency <- as.data.frame(result_pag$results$currency_id)
  mode <- as.data.frame(result_pag$results$buying_mode)
  cond <- as.data.frame(result_pag$results$condition)
  mpago <- as.data.frame(result_pag$results$accepts_mercadopago)
  staten <- as.data.frame(result_pag$results$address$state_name)
  cityn <- as.data.frame(result_pag$results$address$city_name)
  subcat <- as.data.frame(result_pag$results$category_id)
  latitude <- as.data.frame(result_pag$results$location$latitude)
  longitude <- as.data.frame(result_pag$results$location$longitude)
  
  # Atributos
  
  atributos <- matrix(nrow = length(atributos_v))
  atributos[,1] <- atributos_v
  atributos <- as_tibble(atributos)
  atributos <- atributos %>% arrange(V1)
  atributos$value_name <- NA
  
  atributos_col <- atributos %>% pivot_wider(names_from=V1,values_from = value_name)
  
  for(i in 1:nrow(id)){
    
    attr <- result_pag$results$attributes[[i]] %>% select(id,value_name)
    
    attr$id <- tolower(attr$id)
    
    # Diferenciamos por si "faltan o sobran" atributos (en funcion de los primeros 5000)
    
    if(nrow(atributos)>=length(attr$id)){
    
    faltan <- data.frame(atributos %>% filter(!(V1%in%attr$id)) %>% select(V1))
    
    colnames(faltan) <- "id"
    
    faltan$value_name <- NA
    
    attr <- rbind(attr,faltan)
    
    } else {
      
      
    attr <- data.frame(attr %>% filter(id%in%atributos$V1))
      
    }
    
    
    attr <- attr %>% arrange(id)
    
    attr <- attr %>% pivot_wider(names_from=id,values_from = value_name)
    
    atributos_col <- rbind(atributos_col,attr)
    
    
  }
  
  atributos_col <- atributos_col[-1,]
  
    
  #Combina todas las variables y las une al dataframe principal de la categor?a
  variables<-cbind(id,title,price, currency, mode, cond, mpago, staten, cityn, 
                   subcat,latitude,longitude)
  
  colnames(variables) <- c("id","title","price","currency_id","buying_mode",
                           "condition","accepts_mercadopago",
                           "state_name","city_name","category_id",
                           "latitude","longitude")
  
  #Combinamos variables y atributos
  apt<-cbind(variables,atributos_col)
  
  datos_apt <- rbind(datos_apt,apt)
  
  print("Hizo Algo")
  
}


datos_apt <- datos_apt[-1,]

fin <- Sys.time()

tiempo <- fin - inicio

# Falta house
