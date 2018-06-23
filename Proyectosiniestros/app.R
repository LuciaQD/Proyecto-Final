library(shiny)
library(tidyverse)
library(rgdal)
library(raster)
library(maptools)
library(broom)
library(ggmosaic)
library(lubridate)
library(shinythemes)

ui <- fluidPage(theme = shinytheme("sandstone"),
  titlePanel("Siniestros de tránsito en Uruguay"),
  tabsetPanel(
    #Panel de introduccion
    tabPanel("Introducción",
             p("La seguridad vial en Uruguay siempre ha sido un tema de gran importancia....."
               
             ),
             p("La presente aplicacion tiene como objetivo mostrar la influencia de ciertas variables
               en la cantidad de siniestros de transito ocurridos en el periodo 2013-2017.",br(),
               "La base que usamos para esto fue extraida de los datos abiertos de la Unidad Nacional de Seguridad Vial
               juntando datos de los siniestros ocurridos en el periodo mencionado."
               ),
             p("Hola2")
             ),
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
                                                    "Dia de la semana",
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
                                                                    "Colisión entre vehiculos"="COLISIÓN ENTRE VEHÍCULOS",
                                                                    "Caída"="CAÍDA",
                                                                    "Despiste"="DESPISTE",
                                                                    "Atropello de peaton"="ATROPELLO DE PEATÓN",
                                                                    "Atropello de animales"="ATROPELLO DE ANIMALES",
                                                                    "Colision con obstaculo en calzada"="COLISIÓN CON OBSTÁCULO EN CALZADA")),
                 selectInput("selecgrav","Gravedad del siniestro",choices = c("Todas",
                                                                    "Fatal"="FATAL",
                                                                    "Grave"="GRAVE",
                                                                    "Leve"="LEVE",
                                                                    "Sin lesionados"="SIN LESIONADOS"))
                 
               ),
               mainPanel(
                 plotOutput("mapa"),
                 em("Se muestra la cantidad de siniestros cada 100 habitantes en un escala de color en torno a la media.")
               )#Main
             )#Layout
             )#Panel
    
  )#Tabset
  
)#Fluid
 
server <- function(input, output) {
  
  ###Añadir base
  
    output$grafico<-renderPlot(
      if(input$selaño != "Todos"){
        z<-(datos%>%filter(Año==input$selaño))
        if(input$seleccion=="Gravedad")
        {ggplot(z,aes(x = Gravedad)) + 
        geom_bar(fill="seagreen4") +
        labs(x = "Gravedad", y = "Cantidad") + 
        ggtitle("Gravedad de los siniestros")}
        else if(input$seleccion=="Departamento")
        {ggplot(z,aes(x = fct_infreq(Departamento))) + 
            geom_bar(fill="seagreen4") +
            labs(x = "Departamento", y = "Cantidad") +
            coord_flip() + 
            ggtitle("Siniestros por departamento")}
        else if(input$seleccion=="Tipo de siniestro")
        {ggplot(z,aes(x = fct_infreq(Tipo_de_siniestro))) + 
            geom_bar(fill="seagreen4") +
            labs(x = "Tipo de siniestro", y = "Cantidad") + 
            coord_flip() + 
            ggtitle("Tipo de siniestro")}
        else if(input$seleccion=="Mes")
        {z%>%
            group_by(Mes, Año) %>%
            summarize(n = n()) %>%
            ggplot(aes(Mes, n)) +
            geom_line(size = 1 , color = "seagreen4") +
            scale_x_continuous(breaks = seq(1,12,1), 
                               labels = c("Enero", "Febrero", "Marzo", "Abril","Mayo", "Junio", "Julio", "Agosto", "Setiembre", "Octubre", "Noviembre", "Diciembre")) +
            labs(x = "Mes", y = "Cantidad") + 
            ggtitle("Siniestros por mes")}
        else if(input$seleccion=="Dia de la semana")
        {z %>%
            mutate(Dia_semana = wday(Fecha, label = TRUE)) %>%
            ggplot(aes(x = Dia_semana)) + 
            geom_bar(color = "seagreen4") + 
            scale_x_discrete(labels = c("DOMINGO", "LUNES", "MARTES", "MIÉRCOLES", "JUEVES","VIERNES", "SÁBADO" )) +
            labs(x = "Día de la semana", y = "Cantidad") + 
            ggtitle("Siniestros por dia de la semana")
        }
        else if(input$seleccion=="Hora")
        {ggplot(z,aes(x = Hora)) + 
            geom_bar(fill="seagreen4") +
            scale_x_continuous(breaks = seq(0,23,1)) +
            labs(x = "Hora", y = "Cantidad") + 
            ggtitle("Siniestros por hora")}
      }
      else{
        if(input$seleccion=="Gravedad")
        {datos%>%ggplot(aes(x = Gravedad)) + 
          geom_bar(fill="seagreen4") +
          labs(x = "Gravedad", y = "Cantidad") + 
          ggtitle("Gravedad de los siniestros")}
        else if(input$seleccion=="Departamento")
        {datos%>%ggplot(aes(x = fct_infreq(Departamento))) + 
            geom_bar(fill="seagreen4") +
            labs(x = "Departamento", y = "Cantidad") + 
            coord_flip() + 
            ggtitle("Siniestros por departamento")}
        else if(input$seleccion=="Tipo de siniestro")
        {datos%>%ggplot(aes(x = fct_infreq(Tipo_de_siniestro))) + 
            geom_bar(fill="seagreen4") +
            labs(x = "Tipo de siniestro", y = "Cantidad") +
            coord_flip() + 
            ggtitle("Tipo de siniestro")}
        else if(input$seleccion=="Mes")
        {datos %>%
            group_by(Mes, Año) %>%
            summarize(n = n()) %>%
            ggplot(aes(Mes, n, color = factor(Año))) +
            geom_line(size = 1) +
            scale_x_continuous(breaks = seq(1,12,1), 
                               labels = c("Enero", "Febrero", "Marzo", "Abril","Mayo", "Junio", "Julio", "Agosto", "Setiembre", "Octubre", "Noviembre", "Diciembre")) +
            labs(x = "Mes", y = "Cantidad", color = "Año") + 
            ggtitle("Siniestros por mes")}
        else if(input$seleccion=="Dia de la semana")
        {datos %>%
            mutate(Dia_semana = wday(Fecha, label = TRUE)) %>%
            ggplot(aes(x = Dia_semana)) + 
            geom_bar(color = "seagreen4") + 
            scale_x_discrete(labels = c("DOMINGO", "LUNES", "MARTES", "MIÉRCOLES", "JUEVES","VIERNES", "SÁBADO" )) +
            labs(x = "Día de la semana", y = "Cantidad") + 
            ggtitle("Siniestros por dia de la semana")
        }
        else if(input$seleccion=="Hora")
        {datos%>%ggplot(aes(x = Hora)) + 
            geom_bar(fill = "seagreen4") +
            scale_x_continuous(breaks = seq(0,23,1)) +
            labs(x = "Hora", y = "Cantidad") + 
            ggtitle("Siniestros por hora")}
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
            q%>%filter(Tipo_de_siniestro != 'SIN DATOS')%>%
              ggplot(aes(x = Tipo_de_siniestro, fill = Gravedad)) + 
              geom_bar(position = "fill") + 
              scale_color_brewer(palette = "Dark2") +
              coord_flip() +
              labs(x = "Tipo de siniestro", y = "Fracuencia") +
              ggtitle("Tipo de siniestro segun gravedad")}
          else if(input$choice1 == "Gravedad" & input$choice2 == "Tipo de siniestro"){
            q%>%filter(Tipo_de_siniestro != 'SIN DATOS')%>%
              ggplot(aes(x =Gravedad , fill =  Tipo_de_siniestro)) + 
              geom_bar(position = "fill") + 
              scale_color_brewer(palette = "Dark2") +
              coord_flip() +
              labs(x = "Gravedad", y = "Fracuencia") +
              ggtitle("Gravedad segun tipo de siniestro")}
          else if(input$choice1 == "Tipo de siniestro" & input$choice2 == "Tipo de siniestro"){
            q%>%filter(Tipo_de_siniestro != 'SIN DATOS')%>%
            ggplot(aes(x = fct_infreq(Tipo_de_siniestro))) + 
              geom_bar(fill="seagreen4") +
              labs(x = "Tipo de siniestro", y = "Cantidad") + 
              coord_flip() + 
              ggtitle("Tipo de siniestro")
          }
          else if(input$choice1 == "Gravedad" & input$choice2 == "Gravedad"){
          q%>%filter(Tipo_de_siniestro != 'SIN DATOS')%>%
            ggplot(aes(x = fct_infreq(Gravedad))) + 
            geom_bar(fill="seagreen4") +
            labs(x = "Gravedad", y = "Cantidad") + 
            coord_flip() + 
            ggtitle("Gravedad")
          }
        }#If de no departamento
        else if(input$choice1 == "Departamento"){
          if(input$choice2 == "Tipo de siniestro"){
            q%>%filter(Tipo_de_siniestro != 'SIN DATOS')%>%
            ggplot(aes(x = Departamento, fill = Tipo_de_siniestro)) + 
              geom_bar(position = "fill") +
              scale_color_brewer(palette = "Dark2") +
              coord_flip() +
              labs(x = "Departamento", y = "Frecuencia")
          }
          else if(input$choice2 == "Gravedad"){
            q%>%ggplot(aes(x = Departamento, fill = Gravedad)) + 
              geom_bar(position = "fill") +
              scale_color_brewer(palette = "Dark2") +
              coord_flip() +
              labs(x = "Departamento", y = "Frecuencia")
          }
        }#If de si departamento
          

        

      }#If de años
      else if(input$añoboton == "Todos"){
        q<-datos
        if(ch1() != "Departamento"){
          if(input$choice1 == "Tipo de siniestro" & input$choice2 == "Gravedad"){
            q%>%filter(Tipo_de_siniestro != 'SIN DATOS')%>%
              ggplot(aes(x = Tipo_de_siniestro, fill = Gravedad)) + 
              geom_bar(position = "fill") + 
              scale_color_brewer(palette = "Dark2") +
              coord_flip() +
              labs(x = "Tipo de siniestro", y = "Fracuencia") +
              ggtitle("Tipo de siniestro segun gravedad")}
          else if(input$choice1 == "Gravedad" & input$choice2 == "Tipo de siniestro"){
            q%>%filter(Tipo_de_siniestro != 'SIN DATOS')%>%
              ggplot(aes(x =Gravedad , fill =  Tipo_de_siniestro)) + 
              geom_bar(position = "fill") + 
              scale_color_brewer(palette = "Dark2") +
              coord_flip() +
              labs(x = "Gravedad", y = "Fracuencia") +
              ggtitle("Gravedad segun tipo de siniestro")}
          else if(input$choice1 == "Tipo de siniestro" & input$choice2 == "Tipo de siniestro"){
            q%>%filter(Tipo_de_siniestro != 'SIN DATOS')%>%
              ggplot(aes(x = fct_infreq(Tipo_de_siniestro))) + 
              geom_bar(fill="seagreen4") +
              labs(x = "Tipo de siniestro", y = "Cantidad") + 
              coord_flip() + 
              ggtitle("Tipo de siniestro")
          }
          else if(input$choice1 == "Gravedad" & input$choice2 == "Gravedad"){
            q%>%filter(Tipo_de_siniestro != 'SIN DATOS')%>%
              ggplot(aes(x = fct_infreq(Gravedad))) + 
              geom_bar(fill="seagreen4") +
              labs(x = "Gravedad", y = "Cantidad") + 
              coord_flip() + 
              ggtitle("Gravedad")
          }
        }#If de no departamento
        else if(input$choice1 == "Departamento"){
          if(input$choice2 == "Tipo de siniestro"){
            q%>%filter(Tipo_de_siniestro != 'SIN DATOS')%>%
              ggplot(aes(x = Departamento, fill = Tipo_de_siniestro)) + 
              geom_bar(position = "fill") +
              scale_color_brewer(palette = "Dark2") +
              coord_flip() +
              labs(x = "Departamento", y = "Frecuencia")
          }
          else if(input$choice2 == "Gravedad"){
            q%>%ggplot(aes(x = Departamento, fill = Gravedad)) + 
              geom_bar(position = "fill") +
              scale_color_brewer(palette = "Dark2") +
              coord_flip() +
              labs(x = "Departamento", y = "Frecuencia")
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
