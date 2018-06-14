
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
    
    
    
    
    
    
    tabPanel("Mapa"),
    tabPanel("Predicción")
  )#Tabset
  
)#Fluid
 
server <- function(input, output) {
  
  #Intento de cambio de año
   # v<-reactive({
   #   switch (input$seleccion,
   #     "Departamento" = Departamento,
   #     "Gravedad" = Gravedad
   #   )
   # })
  
  
    output$grafico<-renderPlot(
      if(input$selaño != "Todos"){
        z<-(datos%>%filter(Año==input$selaño))
        if(input$seleccion=="Gravedad")
        {ggplot(z,aes(x = Gravedad)) + 
        geom_bar(fill="seagreen4") +
        labs(x = "v", y = "Cantidad")}
        else if(input$seleccion=="Departamento")
        {ggplot(z,aes(x = Departamento)) + 
            geom_bar(fill="seagreen4") +
            labs(x = "Departamento", y = "Cantidad") +
            coord_flip()}
        else if(input$seleccion=="Tipo de siniestro")
        {ggplot(z,aes(x = Tipo_de_siniestro)) + 
            geom_bar(fill="seagreen4") +
            labs(x = "Tipo de siniestro", y = "Cantidad") + 
            coord_flip()}
        else if(input$seleccion=="Mes")
        {ggplot(z,aes(x = Mes)) + 
            geom_bar(fill="seagreen4") +
            scale_x_continuous(breaks = seq(1,12,1), 
                               labels = c("Enero", "Febrero", "Marzo", "Abril","Mayo", "Junio", "Julio", "Agosto", "Setiembre", "Octubre", "Noviembre", "Diciembre")) +
            labs(x = "Mes", y = "Cantidad")}
        else if(input$seleccion=="Dia de la semana")
        {ggplot(z,aes(x = Dia_semana)) + 
            geom_bar(fill="seagreen4") +
            labs(x = "Dia de la semana", y = "Cantidad")}
        else if(input$seleccion=="Hora")
        {ggplot(z,aes(x = Hora)) + 
            geom_bar(fill="seagreen4") +
            scale_x_continuous(breaks = seq(0,23,1)) +
            labs(x = "Hora", y = "Cantidad")}
      }
      else{
        if(input$seleccion=="Gravedad")
        {datos%>%ggplot(aes(x = Gravedad)) + 
          geom_bar() +
          labs(x = "Gravedad del siniestro", y = "Cantidad")}
        else if(input$seleccion=="Departamento")
        {datos%>%ggplot(aes(x = Departamento)) + 
            geom_bar() +
            labs(x = "Departamento", y = "Cantidad")}
        else if(input$seleccion=="Tipo de siniestro")
        {datos%>%ggplot(aes(x = Tipo_de_siniestro)) + 
            geom_bar() +
            labs(x = "Tipo de siniestro", y = "Cantidad")}
        else if(input$seleccion=="Mes")
        {datos%>%ggplot(aes(x = Mes)) + 
            geom_bar(fill="seagreen4") +
            scale_x_continuous(breaks = seq(1,12,1), 
                               labels = c("Enero", "Febrero", "Marzo", "Abril","Mayo", "Junio", "Julio", "Agosto", "Setiembre", "Octubre", "Noviembre", "Diciembre")) +
            labs(x = "Mes", y = "Cantidad")}
        else if(input$seleccion=="Dia de la semana")
        {datos%>%ggplot(aes(x = Dia_semana)) + 
            geom_bar(fill="seagreen4") +
            labs(x = "Dia de la semana", y = "Cantidad")}
        else if(input$seleccion=="Hora")
        {datos%>%ggplot(aes(x = Hora)) + 
            geom_bar() +
            scale_x_continuous(breaks = seq(0,23,1)) +
            labs(x = "Hora", y = "Cantidad")}
      }
        
    )
    # 
    # output$grafico1<-renderPlot(
    #   {if(input$seleccion == "TS")
    #   
    #   {datos %>%filter(Tipo_de_siniestro != 'SIN DATOS') %>%
    #             ggplot(aes(x = fct_infreq(Tipo_de_siniestro))) + 
    #             geom_bar() +
    #             coord_flip() +
    #             labs(x = "Tipo de siniestro", y = "Cantidad")}
    # 
    #   
    #   
    #   else{if(input$seleccion =="grav")
    #   {datos %>%ggplot(aes(x = Gravedad)) + 
    #       geom_bar() +
    #       labs(x = "Gravedad del siniestro", y = "Cantidad")}
    # 
    #   
    #   }})
    # output$grafico2<-renderPlot({if(input$seleccion=="grav")
    #   {datos %>%ggplot(aes(x = Gravedad)) + 
    #             geom_bar() +
    #             labs(x = "Gravedad del siniestro", y = "Cantidad")}
    # })
    
    
  
}

# Run the application 
shinyApp(ui = ui, server = server)

