library(shiny)
library(tidyverse)

ui <- fluidPage(theme = shinytheme("sandstone"),
  titlePanel("Siniestros de tránsito en Uruguay"),
  tabsetPanel(
    #Panel de introduccion
    tabPanel("Resumen",
        fluidRow(
          column(9,
             br(),
             p(h4(tags$b("Introducción"))),
             p("La seguridad vial en Uruguay siempre ha sido un tema de gran importancia y en el que se ha trabajado de manera constante 
                para lograr reducir los siniestros y sus consecuencias.",
               br(),
               "A partir del año  2007 se crea la Unidad Nacional de Seguridad Vial (UNASEV) con el fin de regular y controlar las actividades 
                relativas al tránsito y la Seguridad Vial en todo el territorio nacional.
                Dicha institución cuenta con los registros de todos los siniestros de tránsito ocurridos en el país desde al año 2011, 
                información que se encuentra disponible para toda la población."
             ),
             p(h4(tags$b("Aplicación"))),
             p("La presente aplicación tiene como objetivo caracterizar los siniestros de tránsito ocurridos en Uruguay en el período 2013-2017
                a través de ciertas variables de interés",
               br(),
               "Presentamos tres visualizaciones distintas con las que buscamos mostrar resultados de interés para el objetivo planteado",
               br(),
               "La primera, Análisis Exploratorio, nos muestra la cantidad de siniestros ocurridos según las distintas variables mostrando así los
               principales motivos y resultados de los siniestros de tránsito",
               "La segunda, Variables Cruzadas, expone los resultados de análizar dos variables al mismo tiempo pudiendo así ver con mayor claridad 
               lo expuesto en la primer visualización.",
               br(),
               "Finalmente, la última visualización le da un enfoque territorial al análisis pudiendo ver los resultados en el mapa de Uruguay por departamento.",
             br(),
             p("Fuente de datos:", a("Página web UNASEV", 
                                     href = "http://aplicaciones.unasev.gub.uy/mapas/AdministracionMapaIndicadores/AdministracionMapaIndicadores_Alta"))

             )),
          column(3,
            img(width = 50, height = 50 , src = "udelar.png"),
            em(h6("El presente trabajo se enmarca dentro de la materia Nuevas tecnologías para el análisis estadístico de datos de la carrera 
                  Licenciatura en Estadística, UDELAR."))
          )
          )),
    #Panel de analisis exploratorio
    tabPanel("Análisis Exploratorio",
             #Layout de sidebar y main
             sidebarLayout(
               
               #Panel lateral
               sidebarPanel(
                            selectInput("seleccion","Elige tu variable",
                                        choices = c("Tipo de siniestro",
                                                    "Gravedad",
                                                    "Mes",
                                                    "Día de la semana",
                                                    "Hora",
                                                    "Departamento"
                                                    )
                                        ),
                            selectInput("selaño","Eleccion de año",choices = c("Todos",
                                                                               "2013",
                                                                               "2014",
                                                                               "2015",
                                                                               "2016",
                                                                               "2017"))
                 
               ),
               #Panel principal
               mainPanel(
                 plotOutput("grafico")
                 
               )#Main
               
             )#Layout
             ),#Panel
    
    tabPanel("Variables cruzadas",
             sidebarLayout(
              sidebarPanel(
                selectInput("choice1","Elige primer variable",
                            choices = c("Tipo de siniestro",
                                        "Gravedad",
                                        "Departamento"
                            )),
                selectInput("choice2","Elige segunda variable",
                            choices = c("Tipo de siniestro",
                                        "Gravedad")),
                radioButtons("añoboton", label = h3("Año"),
                             choices = list("2013", "2014", "2015","2016","2017","Todos"), 
                             selected = "Todos")
                
                            
                
              ),
              mainPanel(
                plotOutput("cruzadas")
              )
             )),
    tabPanel("Mapa",
             sidebarLayout(
               sidebarPanel(
                 selectInput("selecanual","Eleccion de año",choices = c("Todos",
                                                                    "2013",
                                                                    "2014",
                                                                    "2015",
                                                                    "2016",
                                                                    "2017")),
                 selectInput("selects","Tipo de siniestro",choices = c("Todos",
                                                                    "Colisión entre vehículos"="COLISIÓN ENTRE VEHÍCULOS",
                                                                    "Caída"="CAÍDA",
                                                                    "Despiste"="DESPISTE",
                                                                    "Atropello de peatón"="ATROPELLO DE PEATÓN",
                                                                    "Atropello de animales"="ATROPELLO DE ANIMALES",
                                                                    "Colisión con obstáculo en calzada"="COLISIÓN CON OBSTÁCULO EN CALZADA")),
                 selectInput("selecgrav","Gravedad del siniestro",choices = c("Todas",
                                                                    "Fatal"="FATAL",
                                                                    "Grave"="GRAVE",
                                                                    "Leve"="LEVE",
                                                                    "Sin lesionados"="SIN LESIONADOS"))
                 
               ),
               mainPanel(
                 plotOutput("mapa"),
                 em("Se muestra la cantidad de siniestros cada 100 habitantes en un escala de color en torno a la media de las variables seleccionadas.")
               )#Main
             )#Layout
             )#Panel
    
  )#Tabset
  
)#Fluid
 
server <- function(input, output) {
  
  library(shiny)
  library(tidyverse)
  library(rgdal)
  library(raster)
  library(maptools)
  library(broom)
  library(ggmosaic)
  library(lubridate)
  library(shinythemes)
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
  
    output$grafico<-renderPlot(
      if(input$selaño != "Todos"){
        z<-(datos%>%filter(Año==input$selaño))
        if(input$seleccion=="Gravedad")
        {ggplot(z,aes(x = Gravedad)) + 
          geom_bar(fill="seagreen4") +
          labs(x = "Gravedad", y = "Cantidad") + 
          ggtitle("Gravedad de los siniestros") +
          theme(plot.title = element_text(size = rel(1.5)), axis.title = element_text(face = "bold"))
          }
        else if(input$seleccion=="Departamento")
        {ggplot(z,aes(x = fct_infreq(Departamento))) + 
          geom_bar(fill="seagreen4") +
          labs(x = "Departamento", y = "Cantidad") +
          coord_flip() + 
          ggtitle("Siniestros por departamento") +
          theme(plot.title = element_text(size = rel(1.5)), axis.title = element_text(face = "bold"))
          }
        else if(input$seleccion=="Tipo de siniestro")
        {ggplot(z,aes(x = fct_infreq(Tipo_de_siniestro))) + 
          geom_bar(fill="seagreen4") +
          labs(x = "Tipo de siniestro", y = "Cantidad") + 
          coord_flip() + 
          ggtitle("Tipo de siniestro") +
          theme(plot.title = element_text(size = rel(1.5)), axis.title = element_text(face = "bold"))
          }
        else if(input$seleccion=="Mes")
        {z%>%
          group_by(Mes, Año) %>%
          summarize(n = n()) %>%
          ggplot(aes(Mes, n)) +
           geom_line(size = 1 , color = "seagreen4") +
           scale_x_continuous(breaks = seq(1,12,1), 
                               labels = c("Enero", "Febrero", "Marzo", "Abril","Mayo", "Junio", "Julio", "Agosto", "Setiembre", "Octubre", "Noviembre", "Diciembre")) +
           labs(x = "Mes", y = "Cantidad") + 
           ggtitle("Siniestros por mes") +
           theme(plot.title = element_text(size = rel(1.5)), axis.title = element_text(face = "bold"))
          }
        else if(input$seleccion=="Día de la semana")
        {z %>%
          mutate(Dia_semana = wday(as.Date(Fecha, "%d/%m/%Y"), label = TRUE)) %>%
          ggplot(aes(x = Dia_semana)) + 
           geom_bar(fill = "seagreen4") + 
           scale_x_discrete(labels = c("DOMINGO", "LUNES", "MARTES", "MIÉRCOLES", "JUEVES","VIERNES", "SÁBADO" )) +
           labs(x = "Día de la semana", y = "Cantidad") + 
           ggtitle("Siniestros por día de la semana") +
           theme(plot.title = element_text(size = rel(1.5)), axis.title = element_text(face = "bold"))
        }
        else if(input$seleccion=="Hora")
        {ggplot(z,aes(x = Hora)) + 
          geom_bar(fill="seagreen4") +
          scale_x_continuous(breaks = seq(0,23,1)) +
          labs(x = "Hora", y = "Cantidad") + 
          ggtitle("Siniestros por hora") +
          theme(plot.title = element_text(size = rel(1.5)), axis.title = element_text(face = "bold"))
          }
      }
      else{
        if(input$seleccion=="Gravedad")
        {datos%>%
          ggplot(aes(x = Gravedad)) + 
           geom_bar(fill="seagreen4") +
           labs(x = "Gravedad", y = "Cantidad") + 
           ggtitle("Gravedad de los siniestros") +
           theme(plot.title = element_text(size = rel(1.5)), axis.title = element_text(face = "bold"))
          }
        else if(input$seleccion=="Departamento")
        {datos%>%
          ggplot(aes(x = fct_infreq(Departamento))) + 
           geom_bar(fill="seagreen4") +
           labs(x = "Departamento", y = "Cantidad") + 
           coord_flip() + 
           ggtitle("Siniestros por departamento") +
           theme(plot.title = element_text(size = rel(1.5)), axis.title = element_text(face = "bold"))
          }
        else if(input$seleccion=="Tipo de siniestro")
        {datos%>%
          ggplot(aes(x = fct_infreq(Tipo_de_siniestro))) + 
           geom_bar(fill="seagreen4") +
           labs(x = "Tipo de siniestro", y = "Cantidad") +
           coord_flip() + 
           ggtitle("Tipo de siniestro") +
           theme(plot.title = element_text(size = rel(1.5)), axis.title = element_text(face = "bold"))
          }
        else if(input$seleccion=="Mes")
        {datos %>%
          group_by(Mes, Año) %>%
          summarize(n = n()) %>%
          ggplot(aes(Mes, n, color = factor(Año))) +
           geom_line(size = 1) +
           scale_x_continuous(breaks = seq(1,12,1), 
                               labels = c("Enero", "Febrero", "Marzo", "Abril","Mayo", "Junio", "Julio", "Agosto", "Setiembre", "Octubre", "Noviembre", "Diciembre")) +
           labs(x = "Mes", y = "Cantidad", color = "Año") + 
           ggtitle("Siniestros por mes") +
           theme(plot.title = element_text(size = rel(1.5)), axis.title = element_text(face = "bold"))
          }
        else if(input$seleccion=="Día de la semana")
        {datos %>%
            mutate(Dia_semana = wday(as.Date(Fecha, "%d/%m/%Y"), label = TRUE)) %>%
            ggplot(aes(x = Dia_semana)) + 
            geom_bar(fill = "seagreen4") + 
            scale_x_discrete(labels = c("DOMINGO", "LUNES", "MARTES", "MIÉRCOLES", "JUEVES","VIERNES", "SÁBADO" )) +
            labs(x = "Día de la semana", y = "Cantidad") + 
            ggtitle("Siniestros por día de la semana") +
            theme(plot.title = element_text(size = rel(1.5)), axis.title = element_text(face = "bold"))
        }
        else if(input$seleccion=="Hora")
        {datos%>%
          ggplot(aes(x = Hora)) + 
           geom_bar(fill = "seagreen4") +
           scale_x_continuous(breaks = seq(0,23,1)) +
           labs(x = "Hora", y = "Cantidad") + 
           ggtitle("Siniestros por hora") +
           theme(plot.title = element_text(size = rel(1.5)), axis.title = element_text(face = "bold"))
          }
      }
        
    )
    
    x<-reactive({input$selecanual})
    y<-reactive({input$selects})
    z<-reactive({input$selecgrav})
    pob<-c(73378,520187,84698,123203,57088,25050,67048,58815,164300,1319108,113124,54765,103493,68088,124878,108309,82595,90053,48134)
    source("mapa.R")
    ch1<-reactive({input$choice1})
    ch2<-reactive({input$choice2})
    añ<-reactive({input$añoboton})
    Departamento <- c("ARTIGAS","CANELONES","CERRO LARGO","COLONIA","DURAZNO","FLORES","FLORIDA","LAVALLEJA","MALDONADO","MONTEVIDEO","PAYSANDU","RIO NEGRO","RIVERA","ROCHA","SALTO","SAN JOSE","SORIANO","TACUAREMBO","TREINTA Y TRES")
    dep <- as.data.frame(Departamento)
    
  
    output$cruzadas<-renderPlot(
      if(input$añoboton != "Todos"){
        q<-(datos%>%filter(Año==input$añoboton))
        if(ch1() != "Departamento"){
          if(input$choice1 == "Tipo de siniestro" & input$choice2 == "Gravedad"){
            q%>%
              ggplot(aes(x = Tipo_de_siniestro, fill = Gravedad)) + 
              geom_bar(position = "fill") + 
              scale_fill_brewer(palette = "Dark2") +
              coord_flip() +
              labs(x = "Tipo de siniestro", y = "Frecuencia") +
              ggtitle("Tipo de siniestro según gravedad") +
              theme(legend.title = element_text(face = "bold"), legend.position = "bottom", plot.title = element_text(size = rel(1.5)), axis.title = element_text(face = "bold"))
            }
          else if(input$choice1 == "Gravedad" & input$choice2 == "Tipo de siniestro"){
            q%>%
              ggplot(aes(x =Gravedad , fill =  Tipo_de_siniestro)) + 
               geom_bar(position = "fill") + 
               scale_fill_brewer(palette = "Dark2", name = "Tipo de siniestro") +
               coord_flip() +
               labs(x = "Gravedad", y = "Frecuencia") +
               ggtitle("Gravedad según tipo de siniestro") +
               theme(legend.title = element_text(face = "bold"), legend.position = "bottom", plot.title = element_text(size = rel(1.5)), axis.title = element_text(face = "bold"))
            }
          else if(input$choice1 == "Tipo de siniestro" & input$choice2 == "Tipo de siniestro"){
            q%>%
             ggplot(aes(x = fct_infreq(Tipo_de_siniestro))) + 
              geom_bar(fill="seagreen4") +
              labs(x = "Tipo de siniestro", y = "Cantidad") + 
              coord_flip() + 
              ggtitle("Tipo de siniestro") +
              theme(plot.title = element_text(size = rel(1.5)), axis.title = element_text(face = "bold"))
          }
          else if(input$choice1 == "Gravedad" & input$choice2 == "Gravedad"){
          q%>%
            ggplot(aes(x = Gravedad)) + 
             geom_bar(fill="seagreen4") +
             labs(x = "Gravedad", y = "Cantidad") + 
             ggtitle("Gravedad de los siniestros") +
             theme(plot.title = element_text(size = rel(1.5)), axis.title = element_text(face = "bold"))  
          }
        }#If de no departamento
        else if(input$choice1 == "Departamento"){
          if(input$choice2 == "Tipo de siniestro"){
            q%>%
             ggplot(aes(x = Departamento, fill = Tipo_de_siniestro)) + 
              geom_bar(position = "fill") +
              scale_fill_brewer(palette = "Dark2", name = "Tipo de siniestro") +
              coord_flip() +
              labs(x = "Departamento", y = "Frecuencia") +
              ggtitle("Departamento según tipo de siniestro") +
              theme(legend.title = element_text(face = "bold"), legend.position = "bottom", plot.title = element_text(size = rel(1.5)), axis.title = element_text(face = "bold"))
          }
          else if(input$choice2 == "Gravedad"){
            q%>%
             ggplot(aes(x = Departamento, fill = Gravedad)) + 
              geom_bar(position = "fill") +
              scale_fill_brewer(palette = "Dark2") +
              coord_flip() +
              labs(x = "Departamento", y = "Frecuencia") +
              ggtitle("Departamento según gravedad") +
              theme(legend.title = element_text(face = "bold"), legend.position = "bottom", plot.title = element_text(size = rel(1.5)), axis.title = element_text(face = "bold"))
          }
        }#If de si departamento
          

        

      }#If de años
      else if(input$añoboton == "Todos"){
        q<-datos
        if(ch1() != "Departamento"){
          if(input$choice1 == "Tipo de siniestro" & input$choice2 == "Gravedad"){
            q%>%
             ggplot(aes(x = Tipo_de_siniestro, fill = Gravedad)) + 
              geom_bar(position = "fill") + 
              scale_fill_brewer(palette = "Dark2") +
              coord_flip() +
              labs(x = "Tipo de siniestro", y = "Frecuencia") +
              ggtitle("Tipo de siniestro según gravedad") +
              theme(legend.title = element_text(face = "bold"), legend.position = "bottom", plot.title = element_text(size = rel(1.5)), axis.title = element_text(face = "bold"))
            }
          else if(input$choice1 == "Gravedad" & input$choice2 == "Tipo de siniestro"){
            q%>%
             ggplot(aes(x =Gravedad , fill =  Tipo_de_siniestro)) + 
              geom_bar(position = "fill") + 
              scale_fill_brewer(palette = "Dark2", name = "Tipo de siniestro") +
              coord_flip() +
              labs(x = "Gravedad", y = "Frecuencia") +
              ggtitle("Gravedad según tipo de siniestro") +
              theme(legend.title = element_text(face = "bold"), legend.position = "bottom", plot.title = element_text(size = rel(1.5)), axis.title = element_text(face = "bold"))
            }
          else if(input$choice1 == "Tipo de siniestro" & input$choice2 == "Tipo de siniestro"){
            q%>%
             ggplot(aes(x = fct_infreq(Tipo_de_siniestro))) + 
              geom_bar(fill="seagreen4") +
              labs(x = "Tipo de siniestro", y = "Cantidad") + 
              coord_flip() + 
              ggtitle("Tipo de siniestro") +
              theme(plot.title = element_text(size = rel(1.5)), axis.title = element_text(face = "bold"))
          }
          else if(input$choice1 == "Gravedad" & input$choice2 == "Gravedad"){
            q%>%
             ggplot(aes(x = Gravedad)) + 
              geom_bar(fill="seagreen4") +
              labs(x = "Gravedad", y = "Cantidad") +
              ggtitle("Gravedad de los siniestros") +
              theme(plot.title = element_text(size = rel(1.5)), axis.title = element_text(face = "bold"))
          }
        }#If de no departamento
        else if(input$choice1 == "Departamento"){
          if(input$choice2 == "Tipo de siniestro"){
            q%>%
             ggplot(aes(x = Departamento, fill = Tipo_de_siniestro)) + 
              geom_bar(position = "fill") +
              scale_fill_brewer(palette = "Dark2", name = "Tipo de siniestro") +
              coord_flip() +
              labs(x = "Departamento", y = "Frecuencia") +
              ggtitle("Departamento según tipo de siniestro") +
              theme(legend.title = element_text(face = "bold"), legend.position = "bottom", plot.title = element_text(size = rel(1.5)), axis.title = element_text(face = "bold"))
          }
          else if(input$choice2 == "Gravedad"){
            q%>%
             ggplot(aes(x = Departamento, fill = Gravedad)) + 
              geom_bar(position = "fill") +
              scale_fill_brewer(palette = "Dark2") +
              coord_flip() +
              labs(x = "Departamento", y = "Frecuencia") +
              ggtitle("Departamento según gravedad") +
              theme(legend.title = element_text(face = "bold"), legend.position = "bottom", plot.title = element_text(size = rel(1.5)), axis.title = element_text(face = "bold"))
          }
        }
      }
    )##Renderplot

    
      
  output$mapa<-renderPlot(
    if(x() == "Todos" & y() == "Todos" & z() == "Todas"){count<-datos%>%group_by(Departamento)%>%summarise(n=n())%>%mutate(n=(n/pob)*100)
    mapa(count)}
    else if(x() == "Todos" & y() != "Todos" & z() == "Todas"){
      count<- datos %>%
        filter(Tipo_de_siniestro == input$selects) %>%
        group_by(Departamento) %>%
      summarise( n=n() ) %>%
        right_join(dep) %>%
        mutate(n = as.numeric(as.character(n))) %>%
        mutate_if(is.numeric,coalesce,0)%>%mutate(n=(n/pob)*100)
      mapa(count)}
    else if(x() != "Todos" & y() == "Todos" & z() == "Todas"){
      count<- datos %>%
        filter(Año == input$selecanual) %>%
        group_by(Departamento) %>%
      summarise( n=n() ) %>%
        right_join(dep) %>%
        mutate(n = as.numeric(as.character(n))) %>%
        mutate_if(is.numeric,coalesce,0)%>%mutate(n=(n/pob)*100)
      mapa(count)}
    else if(x() == "Todos" & y() == "Todos" & z() != "Todas"){
      count<- datos %>%
        filter(Gravedad == input$selecgrav) %>%
        group_by(Departamento) %>%
      summarise( n=n() ) %>%
        right_join(dep) %>%
        mutate(n = as.numeric(as.character(n))) %>%
        mutate_if(is.numeric,coalesce,0)%>%mutate(n=(n/pob)*100)
      mapa(count)}
    else if(x() == "Todos" & y() != "Todos" & z() != "Todas"){
      count<- datos %>%
        filter(Tipo_de_siniestro == input$selects & Gravedad == input$selecgrav) %>%
        group_by(Departamento) %>%
      summarise( n=n() ) %>%
        right_join(dep) %>%
        mutate(n = as.numeric(as.character(n))) %>%
        mutate_if(is.numeric,coalesce,0)%>%mutate(n=(n/pob)*100)
      mapa(count)}
    else if(x() != "Todos" & y() == "Todos" & z() != "Todas"){
      count<- datos %>%
        filter(Año == input$selecanual & Gravedad == input$selecgrav) %>%
        group_by(Departamento) %>%
      summarise( n=n() ) %>%
        right_join(dep) %>%
        mutate(n = as.numeric(as.character(n))) %>%
        mutate_if(is.numeric,coalesce,0)%>%mutate(n=(n/pob)*100)
      mapa(count)}
    else if(x() != "Todos" & y() != "Todos" & z() == "Todas"){
      count<- datos %>%
        filter(Año == input$selecanual & Tipo_de_siniestro == input$selects) %>%
        group_by(Departamento) %>%
      summarise( n=n() ) %>%
        right_join(dep) %>%
        mutate(n = as.numeric(as.character(n))) %>%
        mutate_if(is.numeric,coalesce,0)%>%mutate(n=(n/pob)*100)
      mapa(count)}
    else if(x() != "Todos" & y() != "Todos" & z() != "Todas"){
      count<- datos %>%
        filter(Año == input$selecanual & Tipo_de_siniestro == input$selects & Gravedad == input$selecgrav) %>%
        group_by(Departamento) %>%
      summarise( n=n() ) %>%
        right_join(dep) %>%
        mutate(n = as.numeric(as.character(n))) %>%
        mutate_if(is.numeric,coalesce,0)%>%mutate(n=(n/pob)*100)
      mapa(count)}
  )

    
    
}

# Run the application 
shinyApp(ui = ui, server = server)




