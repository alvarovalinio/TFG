# Script auxiliar donde se hacer un arbol de regresion para la variable precios
# usando como regresores solo la latitud y longitud (filtrando los que son NA)

# Librerias 

library(rpart)
library(rpart.plot)
library(rpart.utils)
library(here)
library(tidyverse)

### DATOS

aptos_yearmonth <- list.files(path = here("mercado_libre/API/datos/limpios/apt"), 
                              pattern = "*.csv", full.names = T)

yearmonth <- c('aptos_202106','aptos_202107',"aptos_202108","aptos_202109","aptos_202110")


aptos <- sapply(aptos_yearmonth, FUN=function(yearmonth){
  read_csv(file=yearmonth)}, simplify=FALSE) %>% bind_rows()


aptos <- aptos %>% group_by(id) %>% 
  arrange(desc(fecha_bajada)) %>%
  slice(1) %>% ungroup()

aptos <- aptos %>% mutate_if(is.character, as.factor)


# Filtramos por el criterio en price:

aptos_todos <- aptos

aptos <- aptos %>% filter(price <= quantile(aptos$price,.95))

# Perdemos esta cantidad de registros

nrow(aptos_todos) - nrow(aptos)


## Obtenemos los datos para el arbol

aptos_lat_lon <- aptos  %>% select(id,city_name,price,latitude,longitude) %>% 
  filter(!is.na(latitude),!is.na(longitude))


# Arbol de regresion

arbol <- rpart(price~ . , data=aptos_lat_lon[,-c(1,2)])

summary(arbol)

# Proceso de poda

broom::tidy(arbol$cptable)

# Grafico de la evolucion del error

cp_error <- data.frame(arbol$cptable)

cp_error %>% ggplot(aes(x=CP,y=xerror))+geom_point(color="red")+geom_line()

# Otra forma

plotcp(arbol) 

# Obtengamos el cp

cp_opt<- arbol$cptable[which.min(arbol$cptable[,"xerror"]),"CP"]

# Arbol podado:

arbol.prune.lat.lon <- rpart(price ~ . , data= aptos_lat_lon[,-c(1,2)],method="anova",
                     control=rpart.control(cp=cp_opt))

## Guardamos el modelo

save(file="arbol_prune_lat_lon.RDS",arbol.prune.lat.lon)

# Grafico

rpart.plot(arbol.prune.lat.lon,roundint = FALSE,digits = 4)

# Reglas en las particiones binarias

rpart.plot::rpart.rules(arbol.prune,digits = 4)

rpart.subrules.table(arbol.prune)

#dataframe having leaf node's rule and subrule combination
rule_df <- rpart.rules.table(arbol.prune) %>%
  filter(Leaf==TRUE) %>%
  group_by(Rule) %>%
  summarise(Subrules = paste(Subrule, collapse=","))

#final dataframe
aptos_lat_lon <- aptos_lat_lon %>%
  mutate(Rule = row.names(arbol.prune$frame)[arbol.prune$where]) %>%
  left_join(rule_df, by="Rule")
head(aptos_lat_lon)

