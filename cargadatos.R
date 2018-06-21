library(readr)
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
  filter(Tipo_de_siniestro != 'SIN DATOS') %>%
  mutate(Año = year(as.Date(Fecha, "%d/%m/%Y")), Mes = month(as.Date(Fecha, "%d/%m/%Y")))
