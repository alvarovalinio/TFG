# Script para obtener los datos por barrior,
# Parametros de entrada: 1) Client ID, 2) Secret ID , 3) Code, 4) redict_url
# , 5) id del barrio (id_city) , 6) apt = T (si es F entonces queremos casas)
# , 7) token =T (devuelve el token para no estar cargando el codigo de nuevo),
#  y 8) atributos =T. 

library(httr)
library(jsonlite)
library(tidyverse)
library(data.table)
library(here)

datos_barrio <- function(clientid,secret,code='primero',redict_url,id_barrio,apt=T,token=T, atributos = T){
  
  
  # Proceso de obtencion de token
  
  # Difenciamos si es el primero que se corre (le pasamos code)
  # O si ya usamos la respuesta del primero
  
  if(length(token)==1){
  response <-  POST(
    'https://api.mercadolibre.com/oauth/token',
    accept_json(),
    authenticate(clientid, secret),
    body = list(client_id=clientid,client_secret=secret,
                grant_type = 'authorization_code',code=code,redirect_uri=redict_url),
    encode = 'form',
    verbose()
  )
  
  
  # Obtenemos el token
  
  mytoken <-  content(response)$access_token
  
  ## Guardamos "the access token", tenemos que pasarselo en cada API request
  HeaderValue <-  paste0('Bearer ', mytoken)
  
  } else {
    
    response <- token
    
    mytoken <-  content(response)$access_token
    
    ## Guardamos "the access token", tenemos que pasarselo en cada API request
    HeaderValue <-  paste0('Bearer ', mytoken)
    
  }
  ### Proceso de obtencion de datos
  
  # Difenciamos si se trata de apartamentos o casas
  
  if(apt==T){
  
    url_aux <- paste0('https://api.mercadolibre.com/sites/MLU/search?category=MLU1474&city=',id_barrio,'&limit=50&offset=')
  
    url_ini <- 'https://api.mercadolibre.com/sites/MLU/search?category=MLU1474&limit=50&offset='
    
    att_item  <- read_delim(here('mercado_libre/API/datos','atributos_item_apt.csv'), delim = ';') %>% 
          filter(Incluir == 'SI') %>% select(id_atributo) %>% mutate(id_atributo = tolower(id_atributo))
    
    } else {
  
    url_aux <- paste0('https://api.mercadolibre.com/sites/MLU/search?category=MLU1468&city=',id_barrio,'&limit=50&offset=')
  
    url_ini <- 'https://api.mercadolibre.com/sites/MLU/search?category=MLU1468&limit=50&offset='
  
    att_item  <- read_delim(here('mercado_libre/API/datos', 'atributos_item_casas.csv'), delim = ';')%>% 
          filter(Incluir == 'SI') %>% select(id_atributo) %>% mutate(id_atributo = tolower(id_atributo))
    }

  
  # Obtenemos lista de los atributos (5000 primeras observaciones)
  
  sequence <- seq(from=0, to=500, by=50)
  
  # Atributos (más variables)
 
  if (atributos[1] == T) {
  
 atributos_v <- vector(mode = 'character', length = 0)
  
  for (s in sequence) {
    request_pag <- GET(url = paste0(url_ini, s), add_headers(Authorization=HeaderValue))
    result_pag <- content(request_pag,type="text",encoding = "UTF-8") %>% fromJSON()
    # este for loop devuelve los atributos dentro de las 50 ibs iniciales
    # hay que adaptarlo para chequeat en 50 mas
    
    for (i in 1:50) {
      for (j in 1:length(result_pag$results$attributes[i][[1]]$id)) {
      
        if (length(result_pag$results$attributes[i][[1]]$id) != 0 && !(result_pag$results$attributes[i][[1]]$id[j] %in% atributos_v) == 'TRUE') {
          atributos_v <- c(atributos_v, result_pag$results$attributes[i][[1]]$id[j])
        }
      
      
    }
  }
 }
 
  atributos_v <- tolower(atributos_v)
  
  #atributos_2<- result_pag$available_filters$id %>% tolower()
  
  #atributos_v <- c(atributos_v,atributos_2[!(atributos_2%in%atributos_v)])
  
  } else {
        atributos_v <- atributos
  }
  # Objeto para ir almacenado los datos
  
  datos<- matrix(ncol = length(atributos_v) + 18 + nrow(att_item))
  colnames(datos) <- c("id","title","price","currency_id","buying_mode",     
                       "condition","accepts_mercadopago",
                       "state_name","city_name","category_id",
                       "latitude","longitude","tags","seller_contact","id_city","id_state","date_created","last_updated"
                       ,sort(tolower(atributos_v)), sort(tolower(att_item$id_atributo)))
  
  datos <- as.data.frame(datos)
  
  
  
  # Vamos obteniendo datos de cada 50 (maximo que deja) y hasta 10000
  
  j <- 0
  
  stop <- 0
  
  while (j<10000 & stop==0){
    
    # URL para moverse de a 50
    url<-paste0(url_aux,j)
    
    # Obtenemos refresh token cada 5000 (por las dudas si corremos mucho tiempo)
    if(j%%5000==0){
      
      refresh_token <- content(response)$refresh_token
      
      response  <-  POST(
        'https://api.mercadolibre.com/oauth/token',
        accept_json(),
        authenticate(clientID, secret),
        body = list(client_id=clientID,client_secret=secret,
                    grant_type = 'refresh_token',refresh_token=refresh_token),
        encode = 'form',
        verbose()
      )
      
      # Ahora guardamos el nuevo token y a su vez tenemos el refresh token para hacer un loop
      
      mytoken <- content(response)$access_token
      
      HeaderValue <-  paste0('Bearer ', mytoken)
      
      print("Obtuvo refresh token")
      
    }
    
    # Obtenemos los 50 de esa pagina
    request_pag<-GET(url=url,
                     add_headers(Authorization=HeaderValue))
    
    # valores de esa pagina
    
    result_pag <- content(request_pag,type="text",encoding = "UTF-8") %>% fromJSON()
    
    if(length(result_pag$results)==0){
      
      stop <- 1
      
    } else {
    
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
    seller_contact <- as.data.frame(result_pag$results$seller_contact$contact)
    id_city <- as.data.frame(result_pag$results$location$city$id)
    id_state <- as.data.frame(result_pag$results$location$state$id)
    
    tags_aux <- lapply(result_pag$results$tags,FUN = function(datos){
      
      
      datos <- datos[str_detect(datos,"quality_picture")]
      
      
      
      if(length(datos)==0){
        
        datos <- NA
      }
      
      return(datos) 
      
    })
    
    tags <- unlist(tags_aux)
    
    # Atributos
    
    atributos <- matrix(nrow = length(atributos_v))
    atributos[,1] <- atributos_v
    atributos <- as.data.frame(atributos)
    atributos <- atributos %>% arrange(V1)
    atributos$value_name <- NA
    
    atributos_col <- atributos %>% pivot_wider(names_from=V1,values_from = value_name)
    
    # Una vez obtenido las variables y atributos, obtenemos date created y last update
    
    atributos_aux <-  sapply(id$`result_pag$results$id`,FUN=function(inmueble){
      
      
      # Obtenemos atributos
      
      attr <- result_pag$results$attributes[[which(id$`result_pag$results$id`==inmueble)]] %>% 
        select(id,value_name)
      
      attr$id <- tolower(attr$id)
      
      # Diferenciamos por si "faltan o sobran" atributos (en funcion de los primeros 5000)
      
      if(nrow(atributos)>length(attr$id)){
        
        
        faltan <- data.frame(atributos %>% filter(!(V1%in%attr$id)) %>% select(V1))
        
        colnames(faltan) <- "id"
        
        faltan$value_name <- NA
        
        attr <- rbind(attr,faltan)
        
        # Descarta los atributos que estan en attr pero no en atributos (caso raro, podria pasar)
        attr <- attr %>% filter(id %in% atributos$V1)
        
      } else {
        
        
        attr <- data.frame(attr %>% filter(id%in%atributos$V1))
        
      }
      
      
      attr <- attr %>% arrange(id)
      
      attr <- attr %>% pivot_wider(names_from=id,values_from = value_name)
      
      # URL descripcion
      
      url_desc <- paste0("https://api.mercadolibre.com/items/",inmueble)
      
      request_desc <- GET(url=url_desc,
                          add_headers(Authorization=HeaderValue))
      
      # valores de esa pagina
      
      result_desc <- content(request_desc,type="text",encoding = "UTF-8") %>% fromJSON()
      
      last <- result_desc$last_updated
      
      create <- result_desc$date_created
      
      #salida <- data.frame(date_created=create,last_updated=last)
      if(length(result_desc$attributes$id) > 0) {
      att <- as.data.table(cbind(result_desc$attributes$id, 
                                 result_desc$attributes$value_name)) %>%
            rename( id_atributo = V1, value_name = V2) %>% 
            mutate(id_atributo = tolower(id_atributo)) %>% 
            filter(id_atributo %in% att_item$id_atributo)
      
      att <- att_item %>% left_join(att, by = 'id_atributo') %>% arrange(id_atributo)
      att <- att %>% pivot_wider(names_from=id_atributo,values_from = value_name)
      } else {
      att <- att_item %>% mutate(value_name = NA) %>% 
            pivot_wider(names_from=id_atributo,values_from = value_name)
      }
      
      return(c(create,last,attr,att))
      
    })
    
    
    atributos_aux <- as.data.frame(t(atributos_aux)) 
    
    atributos_aux <- atributos_aux %>% rename(date_created=V1,last_updated=V2)
    
   
    #Combina todas las variables y las une al dataframe principal de la categor?a
    variables<-cbind(id,title,price, currency, mode, cond, mpago, staten, cityn, 
                     subcat,latitude,longitude,tags,seller_contact,id_city,id_state)
    
    colnames(variables) <- c("id","title","price","currency_id","buying_mode",
                             "condition","accepts_mercadopago",
                             "state_name","city_name","category_id",
                             "latitude","longitude","tags","seller_contact","id_city","id_state")
    
    
  # Hacemos join entre variables y atributos_aux por id
    
  atributos_aux$id<- row.names(atributos_aux)
  
  atributos_aux <- atributos_aux %>% data.table()
  
  variables <- variables %>% data.table()
  
  # Hacemos join
  
  setkey(atributos_aux,id)
  
  setkey(variables,id)
  
  variables_atributos <- merge(variables,atributos_aux,no.match=0)
  
  variables_atributos <- variables_atributos %>% relocate(c(names(datos)))
  
  datos <- rbind(datos,variables_atributos)
  
  print(paste0("Repuesta correcta, va en la iteracion ",j))
    
    j <- j + 50
    
    }  
     
  }
  
  
  datos <- datos[-1,]
  
  datos$dia <- Sys.Date()
  
  
  return(list(datos=datos,response=response, atributos = atributos_v))
  
  
}