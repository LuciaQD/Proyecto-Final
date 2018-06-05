library(readr)

##Armando la base
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

base2013<-rbind(Este_2013,Oeste_2013,Norte_2013,Montevideo_2013)
base2014<-rbind(Este_2014,Oeste_2014,Norte_2014,Montevideo_2014)
base2015<-rbind(Este_2015,Oeste_2015,Norte_2015,Montevideo_2015)
base2016<-rbind(Este_2016,Oeste_2016,Norte_2016,Montevideo_2016)
base2017<-rbind(Este_2017,Oeste_2017,Norte_2017,Montevideo_2017)

datos<-rbind(base2013,base2014,base2015,base2016,base2017)

summary(datos)
