######################################
### Análisis exploratorio de datos ###
######################################

library(here)
library(tidyverse)
library(scales)
library(ggcorrplot)

### Datos ====

source(here("Funciones","funcion_imput_media.R"))

aptos_yearmonth <- list.files(path = here("Datos/Limpios"), 
                              pattern = "*.csv", full.names = T)

yearmonth <- c('aptos_202106','aptos_202107',"aptos_202108", "aptos_202109", "aptos_202110" )

aptos <- sapply(aptos_yearmonth, FUN=function(yearmonth){
      read_csv(file=yearmonth)}, simplify=FALSE) %>% bind_rows()


aptos <- aptos %>% group_by(id) %>% 
      arrange(desc(fecha_bajada)) %>%
      slice(1) %>% ungroup()

aptos <- aptos %>% mutate_if(is.character, as.factor)

# Filtramos por el criterio en price - eliminamos obs. con price superior al percentil 95%

aptos <- aptos %>% filter(price <= quantile(aptos$price,.95))

# vemos prop de NA
p_na <- sapply(aptos, function(x) round(sum(is.na(x))/length(x),4)) %>% data.frame() %>% 
      rename(prop_na=".") %>% arrange(desc(prop_na))


### Precio de oferta ====

# Medidas de resumen precio de oferta
sum.table <- aptos %>% summarise(Min = round(min(price),0),
                                 Q1=round(quantile(price,.25),0),
                                 Mediana = round(median(price),0),
                                 Media = round(mean(price),0),
                                 Q3 = round(quantile(price,.75),0),
                                 Max = round(max(price),0),
                                 Desvio = round(sd(price),0),
                                 CV = round(Desvio/Media,2))

# Histograma precio de oferta
aptos %>% ggplot(aes(x=price)) + 
      geom_histogram(fill = 'mediumblue', alpha = 0.9, bins = 40) +
      theme(axis.ticks.x = element_blank(),
            legend.position = 'none',
            axis.title.y = element_blank(),
            axis.title.x = element_text(face = 'bold', size = 12),
            axis.text.x = element_text(face = 'bold', size = 12),
            axis.text.y = element_text(size = 12)) +
      labs(x = 'Precio de oferta en dólares') +
      scale_fill_manual(values = c('mediumblue')) +
      geom_vline(xintercept = mean(aptos$price, na.rm = TRUE), 
                 colour = 'red', linetype = 'dashed') +
      annotate("text", x = mean(aptos$price, na.rm = TRUE) + 72000, y = 5500, label = paste0("Media = USD ",round(mean(aptos$price, na.rm = TRUE))), color = 'red') +
      scale_x_continuous(label = comma)

### Variables cualitativas ====

# Histograma de la cantidad de dormitorios 
aptos %>% ggplot() +
      geom_bar(aes(x = bedrooms, y = (..count..)/sum(..count..)), fill = 'mediumblue') +
      theme(axis.ticks.x = element_blank(),
            legend.position = 'none',
            axis.title.y = element_blank(),
            axis.title.x = element_text(face = 'bold', size = 10),
            axis.text.x = element_text(face = 'bold')) + 
      scale_y_continuous(labels = scales::percent) +
      geom_text(aes(x = as.factor(bedrooms),label = scales::percent(round((..count..)/sum(..count..), 2)), 
                    y = (..count..)/sum(..count..)), stat = "count", vjust = -0.5, size = 2) + 
      labs(x = 'Cantidad de dormitorios') 

# Histograma de la cantidad de baños completos
aptos %>% ggplot() +
      geom_bar(aes(x = fct_infreq(as.factor(full_bathrooms)), y = (..count..)/sum(..count..)), fill = 'mediumblue') +
      theme(axis.ticks.x = element_blank(),
            legend.position = 'none',
            axis.title.y = element_blank(),
            axis.title.x = element_text(face = 'bold', size = 10),
            axis.text.x = element_text(face = 'bold')) + 
      scale_y_continuous(labels = scales::percent) +
      geom_text(aes(x = as.factor(full_bathrooms),label = scales::percent(round((..count..)/sum(..count..), 2)), 
                    y = (..count..)/sum(..count..)), stat = "count", vjust = -0.5, size = 2) + 
      labs(x = 'Cantidad de baños completos') 

# Histograma de zona Avd. Italia
aptos %>% ggplot() +
      geom_bar(aes(x = fct_infreq(as.factor(zona_avditalia)), y = (..count..)/sum(..count..)), fill = 'mediumblue') +
      theme(axis.ticks.x = element_blank(),
            legend.position = 'none',
            axis.title.y = element_blank(),
            axis.title.x = element_text(face = 'bold', size = 10),
            axis.text.x = element_text(face = 'bold')) + 
      scale_y_continuous(labels = scales::percent) +
      geom_text(aes(x = as.factor(zona_avditalia),label = scales::percent(round((..count..)/sum(..count..), 2)), 
                    y = (..count..)/sum(..count..)), stat = "count", vjust = -0.5, size = 2) + 
      labs(x = 'Zona Avd. Italia') 

# Histograma de el edificio tiene piscina
aptos %>% ggplot() +
      geom_bar(aes(x = fct_infreq(as.factor(has_swimming_pool)), y = (..count..)/sum(..count..)),  fill = 'mediumblue') +
      theme(axis.ticks.x = element_blank(),
            legend.position = 'none',
            axis.title.y = element_blank(),
            axis.title.x = element_text(face = 'bold', size = 10),
            axis.text.x = element_text(face = 'bold')) + 
      scale_y_continuous(labels = scales::percent) +
      geom_text(aes(x = as.factor(has_swimming_pool),label = scales::percent(round((..count..)/sum(..count..), 2)), 
                    y = (..count..)/sum(..count..)), stat = "count", vjust = -0.5, size = 2) + 
      labs(x = 'El edificio tiene piscina') 


### Variables cuantitativas ====

# Gráfico de violín y cajas del precio de oferta según zona Avd. Italia
aptos %>% 
      ggplot(aes(x=as.character(zona_avditalia), y=price)) + 
      geom_violin(fill = 'mediumblue') +
      theme(text = element_text( family = 'sans'),
            axis.title.y = element_text( size = 10),
            axis.title.x = element_text(face = 'bold', size = 10),
            plot.title = element_blank(),
            legend.title = element_blank(),
            axis.text.x = element_text(face = 'bold', size = 10),
            axis.text.y = element_text(face = 'bold', size = 10),
            legend.position = 'none') +
      scale_x_discrete() +
      labs(y='', x='Zona Avd. Italia' ) +
      geom_boxplot(width=0.1, colour='black', outlier.color = 'black', fill = 'mediumblue') +
      scale_y_continuous(labels = comma) 

# Gráfico de violín y cajas del precio de oferta según cantidad de baños completos
aptos %>%
      filter(!is.na(full_bathrooms)) %>%
      ggplot(aes(x=as.character(full_bathrooms), y=price)) + 
      geom_violin(fill = 'mediumblue') +
      theme(text = element_text( family = 'sans'),
            axis.title.y = element_text( size = 10),
            axis.title.x = element_text(face = 'bold', size = 10),
            plot.title = element_blank(),
            legend.title = element_blank(),
            axis.text.x = element_text(face = 'bold', size = 10),
            axis.text.y = element_text(face = 'bold', size = 10),
            legend.position = 'none') +
      scale_x_discrete() +
      labs(y='', x='Cantidad de baños completos' ) +
      geom_boxplot(width=0.1, colour='black', outlier.color = 'black', fill = 'mediumblue') +
      scale_y_continuous(labels = comma) 

# Gráfico de violín y cajas del precio de oferta según cantidad de dormitorios
aptos %>%
      filter(!is.na(bedrooms)) %>%
      ggplot(aes(x=as.character(bedrooms), y=price)) + 
      geom_violin(fill = 'mediumblue') +
      theme(text = element_text( family = 'sans'),
            axis.title.y = element_text( size = 10),
            axis.title.x = element_text(face = 'bold', size = 10),
            plot.title = element_blank(),
            legend.title = element_blank(),
            axis.text.x = element_text(face = 'bold', size = 10),
            axis.text.y = element_text(face = 'bold', size = 10),
            legend.position = 'none') +
      scale_x_discrete() +
      labs(y='', x='Cantidad de dormitorios') +
      geom_boxplot(width=0.1, colour='black', outlier.color = 'black', fill = 'mediumblue') +
      scale_y_continuous(labels = comma) 

# Gráfico de violín y cajas del precio de oferta según el edificio tiene piscina
aptos %>%
      filter(!is.na(has_swimming_pool)) %>%
      ggplot(aes(x=as.character(has_swimming_pool), y=price)) + 
      geom_violin(fill = 'mediumblue') +
      theme(text = element_text( family = 'sans'),
            axis.title.y = element_text( size = 10),
            axis.title.x = element_text(face = 'bold', size = 10),
            plot.title = element_blank(),
            legend.title = element_blank(),
            axis.text.x = element_text(face = 'bold', size = 10),
            axis.text.y = element_text(face = 'bold', size = 10),
            legend.position = 'none') +
      scale_x_discrete() +
      labs(y='', x='El edificio tiene piscina' ) +
      geom_boxplot(width=0.1, colour='black', outlier.color = 'black', fill = 'mediumblue') +
      scale_y_continuous(labels = comma) 

# Gráfico de caja del precio de oferta en dólares según zona Avd. Italia y cantidad de baños completos
aptos %>% ggplot(aes(y=price, group = full_bathrooms, fill = full_bathrooms)) + 
      geom_boxplot() +
      theme(axis.ticks.x = element_blank(),
            legend.position = 'none',
            axis.title.y = element_blank(),
            axis.title.x = element_text(face = 'bold', size = 12),
            axis.text.x = element_blank()) +
      labs(x = 'Precio de oferta en dólares') +
      scale_fill_manual(values = c('orangered2', 'springgreen4')) +
      facet_grid(full_bathrooms~zona_avditalia) +
      scale_y_continuous(label = comma)


# Gráfico de caja del precio de oferta en dólares según distancia a la rambla Este y cantidad de baños completos
# Distancia a la rambla Este discretizada
aptos %>% 
      mutate(dist_rambla = factor(case_when(dist_rambla < quantile(dist_rambla, prob = 0.25) ~ '1er cuantil',
                                            dist_rambla < quantile(dist_rambla, prob = 0.5) ~ 'Entre el 1er y  2do cuantil',
                                            dist_rambla < quantile(dist_rambla, prob = 0.75) ~ 'Entre el 2do y 3er cuantil',
                                            TRUE ~ 'Mayor al tercer cuantil'))) %>%
      ggplot(aes(y=price,x=dist_rambla,fill=full_bathrooms)) + 
      geom_boxplot() +      
      theme(axis.ticks.x = element_blank(),
            axis.title.y = element_blank(),
            axis.title.x = element_text(face = 'bold'),
            axis.text.x = element_text(angle = 45, size = 12),
            legend.title = element_text(face = 'bold')) +
      labs(y = 'Precio de oferta en dólares', x = 'Distancia al centro comercial más cercano') +
      scale_fill_manual('Cantidad de baños \n completos', values = c('darkgreen', 'darkmagenta'))  +
      scale_y_continuous(label = comma) +
      scale_x_discrete(labels = c('1er cuantil','Entre el 1er y \n 2do cuantil','Entre el 2do y \n 3er cuantil','Mayor al tercer \n cuantil')) 


### Gráfico de matriz de correlación de variables cualitativas ====

aptos_cor <- aptos %>% select(price, dist_shop, dist_rambla, covered_area,
                              total_area, no_covered_area) 

corr<-round(cor(aptos_cor, use='pairwise.complete.obs') , 2)

ggcorrplot(corr, method = c("square"),type=c("upper"), 
           ggtheme = ggplot2::theme_gray,lab=TRUE) +
      ggplot2::scale_x_discrete(labels = c('Precio de oferta \n en dólares','Distancia al \n centro  comercial \n más cercano','Distancia a la \n rambla Este','Área \n total','Área \n cubierta','Área \n no cubierta')) +
      ggplot2::scale_y_discrete(labels = c('Distancia al \n centro  comercial \n más cercano', 'Distancia a la \n rambla Este',  'Área \n cubierta', 'Área \n total', 'Área \n no cubierta')) +
      ggplot2::theme(axis.text = element_text(vjust = 0.5)) +
      theme(axis.text = element_text(face = 'bold', size = 6),
            legend.title = element_text(face = 'bold')) 

