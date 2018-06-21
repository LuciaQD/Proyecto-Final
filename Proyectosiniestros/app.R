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
             p("Primera frase",br(),
               "segunda frase"),
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
    
  
    # output$cruzadas<-renderPlot(
    #   if(input$añoboton != "Todos"){
    #     q<-(datos%>%filter(Año==input$selaño))
    #     if())
    #   }
    # )
    
      
  output$mapa<-renderPlot(
    if(x() == "Todos" & y() == "Todos" & z() == "Todas"){count<-datos%>%group_by(Departamento)%>%summarise(n=n())%>%mutate(n=(n/pob)*100)
    mapa(count)}
    else if(x() == "Todos" & y() != "Todos" & z() == "Todas"){
    count<-datos%>%filter(Tipo_de_siniestro==input$selects)%>%group_by(Departamento)%>%summarise(n=n())%>%mutate(n=(n/pob)*100)    
    mapa(count)}
    else if(x() != "Todos" & y() == "Todos" & z() == "Todas"){
    count<-datos%>%filter(Año == input$selecanual)%>%group_by(Departamento)%>%summarise(n=n())%>%mutate(n=(n/pob)*100)    
    mapa(count)}
    else if(x() == "Todos" & y() == "Todos" & z() != "Todas"){
    count<-datos%>%filter(Gravedad == input$selecgrav)%>%group_by(Departamento)%>%summarise(n=n())%>%mutate(n=(n/pob)*100)    
    mapa(count)}
    else if(x() == "Todos" & y() != "Todos" & z() != "Todas"){
    count<-datos%>%filter(Tipo_de_siniestro == input$selects & Gravedad == input$selecgrav)%>%group_by(Departamento)%>%summarise(n=n())%>%mutate(n=(n/pob)*100)
    mapa(count)}
    else if(x() != "Todos" & y() == "Todos" & z() != "Todas"){
    count<-datos%>%filter(Gravedad == input$selecgrav & Año == input$selecanual)%>%group_by(Departamento)%>%summarise(n=n())%>%mutate(n=(n/pob)*100)
    mapa(count)}
    else if(x() != "Todos" & y() != "Todos" & z() == "Todas"){
    count<-datos%>%filter(Año == input$selecanual & Tipo_de_siniestro == input$selects)%>%group_by(Departamento)%>%summarise(n=n())%>%mutate(n=(n/pob)*100)
    mapa(count)}
    else if(x() != "Todos" & y() != "Todos" & z() != "Todas"){
    count<-datos%>%filter(Año == input$selecanual & Gravedad == input$selecgrav & Tipo_de_siniestro == input$selects)%>%group_by(Departamento)%>%summarise(n=n())%>%mutate(n=(n/pob)*100)
    mapa(count)}
  )
   
    
    
}

# Run the application 
shinyApp(ui = ui, server = server)
