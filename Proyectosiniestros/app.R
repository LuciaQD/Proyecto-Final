
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
                            selectInput("seleccion","Elige tu grafico",
                                        choices = c("Tipo de siniestro" = "TS",
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
                                        )
                 
               ),
               #Panel principal
               mainPanel(
                 conditionalPanel(condition = "input.seleccion == TS",
                                  plotOutput("grafico1")),
                 conditionalPanel(condition = "input.seleccion == grav",
                                  plotOutput("grafico2")),
                 conditionalPanel(condition = "input.seleccion == SA",
                                  plotOutput("grafico3"))
                 
               )#Main
               
             )#Layout
             ),#Panel
    
    
    
    
    
    
    tabPanel("Mapa"),
    tabPanel("Predicción")
  )#Tabset
  
)#Fluid
 
server <- function(input, output) {
  
  
    
    
    output$grafico1<-renderPlot({if(input$seleccion == "TS")
      
      {datos %>%filter(Tipo_de_siniestro != 'SIN DATOS') %>%
                ggplot(aes(x = fct_infreq(Tipo_de_siniestro))) + 
                geom_bar() +
                coord_flip() +
                labs(x = "Tipo de siniestro", y = "Cantidad")}
    }
      
      )
    output$grafico2<-renderPlot({if(input$seleccion=="grav")
      {datos %>%ggplot(aes(x = Gravedad)) + 
                geom_bar() +
                labs(x = "Gravedad del siniestro", y = "Cantidad")}
    })
    
    output$grafico3<-renderPlot({if(input$seleccion=="SA")
    {datos %>%
        ggplot(aes(x = Año)) + 
        geom_bar() +
        labs(x = "Año", y = "Cantidad") }
    })
    
  
}

# Run the application 
shinyApp(ui = ui, server = server)

