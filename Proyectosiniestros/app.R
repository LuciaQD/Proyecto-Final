
library(shiny)

ui <- fluidPage(
  titlePanel("Siniestros en el periodo 2013-2017"),
  tabsetPanel(
    #Panel de introduccion
    tabPanel("Introducción",
             p("Primera frase",br(),
               "segunda frase"),
             p("Hola2")
             ),
    #Panel de analisis exploratorio
    tabPanel("Analisis Exploratorio",
             #Layout de sidebar y main
             sidebarLayout(
               
               #Panel lateral
               sidebarPanel(
                            selectInput("seleccion","Elige tu variable",
                                        choices = c("TS",
                                                    "Gravedad"="grav",
                                                    "Siniestros por año" = "SA",
                                                    "Siniestro por mes",#elegir bar o line
                                                    "Siniestros por dia",
                                                    "Siniestros por hora",
                                                    "Siniestros por departamento",
                                                    "Siniestros por mes y por año",
                                                    "Tipo de siniestro por año",
                                                    "Gravedad segun tipo de siniestro",
                                                    "Gravedad del siniestro por departamento",
                                                    "Gravedad del siniestro por año",
                                                    "Gravedad por mes",
                                                    "Gravedad por dia de la semana"
                                                    )
                                        ),
                            selectInput("selaño","Eleccion de año",choices = c("2013",
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
    
    
    
    
    
    
    tabPanel("Mapa"),
    tabPanel("Predicción")
  )#Tabset
  
)#Fluid
 
server <- function(input, output) {
  
  #Intento de cambio de año
    output$grafico<-renderPlot(
      datos%>%filter(Año==input$selaño)%>%
        ggplot(aes(x = Gravedad)) + 
        geom_bar() +
        labs(x = "Gravedad del siniestro", y = "Cantidad")
    )
    
    output$grafico1<-renderPlot(
      {if(input$seleccion == "TS")
      
      {datos %>%filter(Tipo_de_siniestro != 'SIN DATOS') %>%
                ggplot(aes(x = fct_infreq(Tipo_de_siniestro))) + 
                geom_bar() +
                coord_flip() +
                labs(x = "Tipo de siniestro", y = "Cantidad")}
    
      
      
      else{if(input$seleccion =="grav")
      {datos %>%ggplot(aes(x = Gravedad)) + 
          geom_bar() +
          labs(x = "Gravedad del siniestro", y = "Cantidad")}
    
      
      }})
    output$grafico2<-renderPlot({if(input$seleccion=="grav")
      {datos %>%ggplot(aes(x = Gravedad)) + 
                geom_bar() +
                labs(x = "Gravedad del siniestro", y = "Cantidad")}
    })
    
    
  
}

# Run the application 
shinyApp(ui = ui, server = server)

