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
          mutate(A?o = year(as.Date(Fecha, "%d/%m/%Y")), Mes = month(as.Date(Fecha, "%d/%m/%Y")))
         

summary(datos)


## Graficos de variables cualitativas count

# Tipo de siniestro en todos los años
a<-datos %>%
  filter(Tipo_de_siniestro != 'SIN DATOS') %>%
  ggplot(aes(x = fct_infreq(Tipo_de_siniestro))) + 
  geom_bar() +
  coord_flip() +
  labs(x = "Tipo de siniestro", y = "Cantidad")
  
# Gravedad de los siniestros en todos los a?os  
b<-datos %>%
  ggplot(aes(x = Gravedad)) + 
  geom_bar() +
  labs(x = "Gravedad del siniestro", y = "Cantidad")

# Siniestros por a?o
c<-datos %>%
  ggplot(aes(x = Año)) + 
  geom_bar() +
  labs(x = "A?o", y = "Cantidad") 

# Siniestros por mes para todos los a?os
d<-datos %>%
  group_by(Mes) %>%
  summarize(n = n()) %>%
  ggplot(aes(Mes, n)) +
  geom_line(size = 1) +
  scale_x_continuous(breaks = seq(1,12,1), 
                     labels = c("Enero", "Febrero", "Marzo", "Abril","Mayo", "Junio", "Julio", "Agosto", "Setiembre", "Octubre", "Noviembre", "Diciembre")) +
  labs(x = "Mes", y = "Cantidad")

# Siniestros por d?a de la semana en todos los a?os
e<-datos %>%
  ggplot(aes(x = Dia_semana)) + 
  geom_bar() + 
  labs(x = "D?a de la semana", y = "Cantidad") # ver sise puedo mostrar la semana ordenada

# Siniestros por hora en todos los a?os
f<-datos %>%
  ggplot(aes(x = Hora)) + 
  geom_bar() +
  scale_x_continuous(breaks = seq(0,23,1)) +
  labs(x = "Hora del siniestro", y = "Cantidad")

# Siniestros por departamento en todos los a?os
g<-datos %>%
  ggplot(aes(x = fct_infreq(Departamento))) + 
  geom_bar() +
  coord_flip() + 
  labs(x = "Departamento", y = "Cantidad")

#Siniestros por mes y por a?o
h<-datos %>%
  group_by(Mes, Año) %>%
  summarize(n = n()) %>%
  ggplot(aes(Mes, n, color = factor(Año))) +
  geom_line(size = 1) +
  scale_x_continuous(breaks = seq(1,12,1), 
                     labels = c("Enero", "Febrero", "Marzo", "Abril","Mayo", "Junio", "Julio", "Agosto", "Setiembre", "Octubre", "Noviembre", "Diciembre")) +
  labs(x = "Mes", y = "Cantidad", color = "Año")

# Tipo de siniestros por a?o
i<-datos %>%
  filter(Tipo_de_siniestro != 'SIN DATOS') %>%
  ggplot(aes(x = Año, fill = Tipo_de_siniestro)) + 
  geom_bar(position = "fill") +
  labs(x = "Año", y = "Frecuencia", fill = "Tipo de siniestro")

# Tipo de siniestro y gravedad en todos los a?os
j<-datos %>%
  filter(Tipo_de_siniestro != 'SIN DATOS') %>%
  ggplot(aes(x = Tipo_de_siniestro, fill = Gravedad)) + 
  geom_bar(position = "fill") + 
  coord_flip() +
  labs(x = "Tipo de siniestro", y = "Fracuencia") 
# theme(legend.position = "bottom", legend.title = element_text(face = "bold"))

# Departamento y gravedad en todos los a?os
k<-datos %>%
  ggplot(aes(x = Departamento, fill = Gravedad)) + 
  geom_bar(position = "fill") +
  coord_flip() +
  labs(x = "Departamento", y = "Frecuencia")

# Gravedad de los siniestros por a?o
l<-datos %>%
  ggplot(aes(x = Año, fill = Gravedad)) + 
  geom_bar(position = "fill") +
  labs(x = "Año", y = "Frecuencia") #Parece que hay una tendencia en los no lesionados

#Siniestros por mes y por gravedad
m<-datos %>%
  group_by(Mes, Gravedad) %>%
  summarize(n = n()) %>%
  ggplot(aes(Mes, n, color = factor(Gravedad))) +
  geom_line(size = 1) +
  scale_x_continuous(breaks = seq(1,12,1), 
                     labels = c("Enero", "Febrero", "Marzo", "Abril","Mayo", "Junio", "Julio", "Agosto", "Setiembre", "Octubre", "Noviembre", "Diciembre")) +
  labs(x = "Mes", y = "Cantidad", color = "Gravedad")

# Gravedad por d?a de la semana
n<-datos %>%
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





