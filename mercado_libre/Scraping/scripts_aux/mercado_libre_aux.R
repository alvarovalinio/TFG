library(rvest)
library(httr)
library(dplyr)
library(purrr)

## Definimos funcion para scrapear de "forma amable" (definamos user-agent?)

read_html_delayed <- slowly(read_html, 
                            rate = rate_delay(runif(1,0.5,1)))

### Obtenemos de todas las propiedades por los departamentos-balnearios que hay en mercado libre

url_general <- "https://www.mercadolibre.com.uy/inmuebles/#menu=categories"

html_general <- read_html_delayed(url_general) 

# Este objeto tiene los links para acceder a las diferentes propiedades segun el dpto(balneario)

links_dept <- html_general %>% 
  html_nodes("h3.featured-loc__heading a.featured-loc__link") %>% html_attr("href")


# Ejemplo : Mvdeo (la idea es realizar el mismo procedimiento para cada dpto)

## Veamos todas las propiedades de mvdeo (la idea es darle a la funcion param con departamento?)

url_mvdeo <- links_dept[3] # Obtenemos el link de cada "imagen", para ir iterando y obteniendo la data

html_mvdeo <- read_html_delayed(url_mvdeo)


# Aca obtenemos el link de las primeras casas, estaria bueno obtener link para las siguientes paginas

links_casas <- html_mvdeo %>% 
  html_nodes("div.ui-search-result__image a.ui-search-link") %>% html_attr("href")


# Ejemplo : (la idea es iterar para cada casa)

# Obtenmos info de una casa

html_casa_1 <- read_html_delayed(links_casas[1])

precio_casa_1 <- html_casa_1 %>% 
                    html_nodes("span.price-tag-fraction") %>% html_text()

direc_casa_1 <- html_casa_1 %>% 
                    html_nodes("h1.item-title__primary") %>% html_text(trim=T) %>% 
                      stringr::str_remove(",")

names_casa_1 <- html_casa_1 %>% 
                      html_nodes("ul.specs-list li.specs-item strong") %>% html_text()

values_casa_1 <- html_casa_1 %>% 
                    html_nodes("ul.specs-list li.specs-item span") %>% html_text()

variables_casa_1 <- t(values_casa_1) %>% data.table::data.table()

data.table::setnames(variables_casa_1,c(names_casa_1)) 

### Para obtener el link de "las siguientas paginas de casas" (sigue ejemplo Mvdeo)

links_sig_pag <- html_mvdeo %>% 
                    html_nodes("li.andes-pagination__button a.andes-pagination__link.ui-search-link") %>% 
                        html_attr("href")

# Obs, solo tenemos de a 10! El 11 es idem al segundo, es como tocar un "Siguiente"
# Comentario : Buscarle la vuelta a esto!