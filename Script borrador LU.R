library(tidyverse)
library(readr)
library(shiny)
library(ggmosaic)
library(lubridate)

## Armamos la base

#1 Subimos los archivos

Este_2013 <- read_csv("Siniestros/2013/Este_2013.txt")
Oeste_2013 <- read_csv("Siniestros/2013/Oeste_2013.txt")
Norte_2013 <- read_csv("Siniestros/2013/Norte_2013.txt")
Montevideo_2013 <- read_csv("Siniestros/2013/Montevideo_2013.txt")

Este_2014 <- read_csv("Siniestros/2014/Este_2014.txt")
Oeste_2014 <- read_csv("Siniestros/2014/Oeste_2014.txt")
Norte_2014 <- read_csv("Siniestros/2014/Norte_2014.txt")
Montevideo_2014 <- read_csv("Siniestros/2014/Montevideo_2014.txt")

Este_2015 <- read_csv("Siniestros/2015/Este_2015.txt")
Oeste_2015 <- read_csv("Siniestros/2015/Oeste_2015.txt")
Norte_2015 <- read_csv("Siniestros/2015/Norte_2015.txt")
Montevideo_2015 <- read_csv("Siniestros/2015/Montevideo_2015.txt")

Este_2016 <- read_csv("Siniestros/2016/Este_2016.txt")
Oeste_2016 <- read_csv("Siniestros/2016/Oeste_2016.txt")
Norte_2016 <- read_csv("Siniestros/2016/Norte_2016.txt")
Montevideo_2016 <- read_csv("Siniestros/2016/Montevideo_2016.txt")

Este_2017 <- read_csv("Siniestros/2017/Este_2017.txt")
Oeste_2017 <- read_csv("Siniestros/2017/Oeste_2017.txt")
Norte_2017 <- read_csv("Siniestros/2017/Norte_2017.txt")
Montevideo_2017 <- read_csv("Siniestros/2017/Montevideo_2017.txt")

#2 Armamos las bases por a?o

base2013 <- bind_rows(Este_2013, Oeste_2013, Norte_2013, Montevideo_2013)
base2014 <- bind_rows(Este_2014, Oeste_2014, Norte_2014, Montevideo_2014) 
base2015 <- bind_rows(Este_2015, Oeste_2015, Norte_2015, Montevideo_2015) 
base2016 <- bind_rows(Este_2016, Oeste_2016, Norte_2016, Montevideo_2016)  
base2017 <- bind_rows(Este_2017,Oeste_2017,Norte_2017,Montevideo_2017)

#3 Armamos la base de todos los a?os, cambiamos los nombres de las variables con espacio en el mismo y agregamos las variables A?o y Mes 

datos <- as.data.frame(bind_rows(base2013, base2014, base2015, base2016, base2017)) %>% 
          rename_at(3, ~"Tipo_de_siniestro") %>%  
          rename_at(5, ~ "Dia_semana") %>%
          mutate(Año = year(as.Date(Fecha, "%d/%m/%Y")), Mes = month(as.Date(Fecha, "%d/%m/%Y")))
         

summary(datos)


## Graficos de variables cualitativas count

# Tipo de siniestro en todos los años
datos %>%
  filter(Tipo_de_siniestro != 'SIN DATOS') %>%
  ggplot(aes(x = fct_infreq(Tipo_de_siniestro))) + 
  geom_bar() +
  coord_flip() +
  labs(x = "Tipo de siniestro", y = "Cantidad")
  
# Gravedad de los siniestros en todos los a?os  
datos %>%
  ggplot(aes(x = Gravedad)) + 
  geom_bar() +
  labs(x = "Gravedad del siniestro", y = "Cantidad")

# Siniestros por a?o
datos %>%
  ggplot(aes(x = Año)) + 
  geom_bar() +
  labs(x = "A?o", y = "Cantidad") 

# Siniestros por mes para todos los a?os
datos %>%
  group_by(Mes) %>%
  summarize(n = n()) %>%
  ggplot(aes(Mes, n)) +
  geom_line(size = 1) +
  scale_x_continuous(breaks = seq(1,12,1), 
                     labels = c("Enero", "Febrero", "Marzo", "Abril","Mayo", "Junio", "Julio", "Agosto", "Setiembre", "Octubre", "Noviembre", "Diciembre")) +
  labs(x = "Mes", y = "Cantidad")

# Siniestros por d?a de la semana en todos los a?os
datos %>%
  ggplot(aes(x = Dia_semana)) + 
  geom_bar() + 
  labs(x = "D?a de la semana", y = "Cantidad") # ver sise puedo mostrar la semana ordenada

# Siniestros por hora en todos los a?os
datos %>%
  ggplot(aes(x = Hora)) + 
  geom_bar() +
  scale_x_continuous(breaks = seq(0,23,1)) +
  labs(x = "Hora del siniestro", y = "Cantidad")

# Siniestros por departamento en todos los a?os
datos %>%
  ggplot(aes(x = fct_infreq(Departamento))) + 
  geom_bar() +
  coord_flip() + 
  labs(x = "Departamento", y = "Cantidad")

#Siniestros por mes y por a?o
datos %>%
  group_by(Mes, Año) %>%
  summarize(n = n()) %>%
  ggplot(aes(Mes, n, color = factor(Año))) +
  geom_line(size = 1) +
  scale_x_continuous(breaks = seq(1,12,1), 
                     labels = c("Enero", "Febrero", "Marzo", "Abril","Mayo", "Junio", "Julio", "Agosto", "Setiembre", "Octubre", "Noviembre", "Diciembre")) +
  labs(x = "Mes", y = "Cantidad", color = "Año")

# Tipo de siniestros por a?o
datos %>%
  filter(Tipo_de_siniestro != 'SIN DATOS') %>%
  ggplot(aes(x = Año, fill = Tipo_de_siniestro)) + 
  geom_bar(position = "fill") +
  labs(x = "Año", y = "Frecuencia", fill = "Tipo de siniestro")

# Tipo de siniestro y gravedad en todos los a?os
datos %>%
  filter(Tipo_de_siniestro != 'SIN DATOS') %>%
  ggplot(aes(x = Tipo_de_siniestro, fill = Gravedad)) + 
  geom_bar(position = "fill") + 
  coord_flip() +
  labs(x = "Tipo de siniestro", y = "Fracuencia") 
# theme(legend.position = "bottom", legend.title = element_text(face = "bold"))

# Departamento y gravedad en todos los a?os
datos %>%
  ggplot(aes(x = Departamento, fill = Gravedad)) + 
  geom_bar(position = "fill") +
  coord_flip() +
  labs(x = "Departamento", y = "Frecuencia")

# Gravedad de los siniestros por a?o
datos %>%
  ggplot(aes(x = Año, fill = Gravedad)) + 
  geom_bar(position = "fill") +
  labs(x = "Año", y = "Frecuencia") #Parece que hay una tendencia en los no lesionados

#Siniestros por mes y por gravedad
datos %>%
  group_by(Mes, Gravedad) %>%
  summarize(n = n()) %>%
  ggplot(aes(Mes, n, color = factor(Gravedad))) +
  geom_line(size = 1) +
  scale_x_continuous(breaks = seq(1,12,1), 
                     labels = c("Enero", "Febrero", "Marzo", "Abril","Mayo", "Junio", "Julio", "Agosto", "Setiembre", "Octubre", "Noviembre", "Diciembre")) +
  labs(x = "Mes", y = "Cantidad", color = "Gravedad")

# Gravedad por d?a de la semana
datos %>%
  ggplot(aes(x = Dia_semana, fill = Gravedad)) + 
  geom_bar(position = "fill") + 
  labs(x = "Día de la semana", y = "Cantidad")



## Otros gr?ficos
# Mosaico: Gravedad por d?a de la semana
datos %>%
  group_by(Gravedad, Dia_semana) %>%
  summarise(n = n()) %>%
  ggplot() + 
  geom_mosaic(aes(weight = n, x = product(Gravedad, Dia_semana), 
                  color = Gravedad, fill = Gravedad)) +
  theme(axis.text.x = element_text(angle = 90)) #ver porque no queda

# Mosaico de tres variables: Tipo de siniestro, dia de la semana y gravedad
datos %>%
  filter(Tipo_de_siniestro != 'SIN DATOS') %>%
  group_by(Tipo_de_siniestro,Gravedad, Dia_semana) %>%
  summarise(n = n()) %>%
  ggplot() + 
  geom_mosaic(aes(weight = n, x = product(Gravedad, Dia_semana), 
                  color = Tipo_de_siniestro, fill = Tipo_de_siniestro)) +
  theme(axis.text.x = element_text(angle = 90))

##Mapa

count<-datos%>%group_by(Departamento)%>%summarise(n=n())
count<-count$n
uruguay <- getData("GADM", country = "UY", level = 0)

uruguay_states <- getData("GADM", country = "UY", level = 1)

uystates_UTM <-spTransform(uruguay_states, CRS("+init=EPSG:5383")) 
NAME_1 <- uystates_UTM@data$NAME_1
count_df <- data.frame(NAME_1, count)

uystates_UTM@data$id <- rownames(uystates_UTM@data)
uystates_UTM@data <- plyr::join(uystates_UTM@data, count_df, by="NAME_1")
uystates_df <- tidy(uystates_UTM)
uystates_df <- plyr::join(uystates_df,uystates_UTM@data, by="id")

uystates_df <-uystates_df %>% filter(!(NAME_1=="Rivera"& lat<6400000)) #un error en el mapa que hay que sacar
theme_opts <- list(theme(panel.grid.minor = element_blank(),
                         panel.grid.major = element_blank(),
                         panel.background = element_blank(),
                         plot.background = element_blank(),
                         axis.line = element_blank(),
                         axis.text.x = element_blank(),
                         axis.text.y = element_blank(),
                         axis.ticks = element_blank(),
                         axis.title.x = element_blank(),
                         axis.title.y = element_blank(),
                         plot.title = element_blank()))

countunique <-uystates_df %>%
  group_by(NAME_1) %>%
  summarise(mlong = mean(long), mlat = mean(lat))


ggplot() +
  geom_polygon(data = uystates_df, aes(x = long, y = lat, group = group, fill =
                                         count), color = "black", size = 0.25) +
  geom_text(data =countunique ,aes(label = round(count,2), x = mlong, y = mlat))+
  theme(aspect.ratio = 1) + labs(fill = "Cantidad")+
  scale_fill_gradient2( midpoint = mean(count),low = "red", mid = "white",
                        high = "blue") +
  theme_opts


