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
    
    tabPanel("Variables cruzadas"),
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
                                                                    "Colisión entre vehiculos",
                                                                    "Caída",
                                                                    "DESPISTE",
                                                                    "Atropello de peaton",
                                                                    "Atropello de animales",
                                                                    "Colision con obstaculo en calzada")),
                 selectInput("selecgrav","Gravedad del siniestro",choices = c("Todas",
                                                                    "Fatal",
                                                                    "Grave",
                                                                    "Leve",
                                                                    "Sin lesionados"))
                 
               ),
               mainPanel(
                 plotOutput("mapa")
               )#Main
             )#Layout
             ),#Panel
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
    output$mapa<-renderPlot(
      # if(input$selecanual == "Todos" & input$selects == "Todos" & input$selecgrav == "Todos"){count<-datos%>%group_by(Departamento)%>%summarise(n=n())}
      # else if(input$selecanual == "Todos" & input$selects != "Todos" & input$selecgrav == "Todos"){count<-datos%>%filter(Tipo_de_siniestro==input$selects)%>%group_by(Departamento)%>%summarise(n=n())
      # count<-count$n
      # uruguay <- getData("GADM", country = "UY", level = 0)
      # uruguay_states <- getData("GADM", country = "UY", level = 1)
      # uystates_UTM <-spTransform(uruguay_states, CRS("+init=EPSG:5383")) 
      # NAME_1 <- uystates_UTM@data$NAME_1
      # count_df <- data.frame(NAME_1, count)
      # uystates_UTM@data$id <- rownames(uystates_UTM@data)
      # uystates_UTM@data <- plyr::join(uystates_UTM@data, count_df, by="NAME_1")
      # uystates_df <- tidy(uystates_UTM)
      # uystates_df <- plyr::join(uystates_df,uystates_UTM@data, by="id")
      # uystates_df <-uystates_df %>% filter(!(NAME_1=="Rivera"& lat<6400000)) #un error en el mapa que hay que sacar
      # theme_opts <- list(theme(panel.grid.minor = element_blank(),
      #                          panel.grid.major = element_blank(),
      #                          panel.background = element_blank(),
      #                          plot.background = element_blank(),
      #                          axis.line = element_blank(),
      #                          axis.text.x = element_blank(),
      #                          axis.text.y = element_blank(),
      #                          axis.ticks = element_blank(),
      #                          axis.title.x = element_blank(),
      #                          axis.title.y = element_blank(),
      #                          plot.title = element_blank()))
      # 
      # countunique <-uystates_df %>%
      #   group_by(NAME_1) %>%
      #   summarise(mlong = mean(long), mlat = mean(lat))
      # ggplot() +
      #   geom_polygon(data = uystates_df, aes(x = long, y = lat, group = group, fill =
      #                                          count), color = "black", size = 0.25) +
      #   geom_text(data =countunique ,aes(label = round(count,2), x = mlong, y = mlat))+
      #   theme(aspect.ratio = 1) + labs(fill = "Cantidad")+
      #   scale_fill_gradient2( midpoint = mean(count),low = "red", mid = "white",
      #                         high = "blue") +
      #   theme_opts}
      # else if(input$selecanual != "Todos" & input$selects == "Todos" & input$selecgrav == "Todos"){count<-datos%>%filter(Año==input$selecanual)%>%group_by(Departamento)%>%summarise(n=n())}
      # else if(input$selecanual == "Todos" & input$selects == "Todos" & input$selecgrav != "Todos"){count<-datos%>%filter(Gravedad==input$selecgrav)%>%group_by(Departamento)%>%summarise(n=n())}
      # else if(input$selecanual == "Todos" & input$selects != "Todos" & input$selecgrav != "Todos"){count<-datos%>%filter(Gravedad==input$selecgrav & Tipo_de_siniestro==input$selects)%>%group_by(Departamento)%>%summarise(n=n())}
      # else if(input$selecanual != "Todos" & input$selects == "Todos" & input$selecgrav != "Todos"){count<-datos%>%filter(Gravedad==input$selecgrav & Año==input$selecanual)%>%group_by(Departamento)%>%summarise(n=n())}
      # else if(input$selecanual != "Todos" & input$selects != "Todos" & input$selecgrav == "Todos"){count<-datos%>%filter(Año==input$selecanual & Tipo_de_siniestro==input$selects)%>%group_by(Departamento)%>%summarise(n=n())}
      # else if(input$selecanual != "Todos" & input$selects != "Todos" & input$selecgrav != "Todos"){count<-datos%>%filter(Año==input$selecanual & Gravedad==input$selecgrav & Tipo_de_siniestro==input$selects)%>%group_by(Departamento)%>%summarise(n=n())}

      

      ggplot() +
        geom_polygon(data = uystates_df, aes(x = long, y = lat, group = group, fill =
                                               count), color = "black", size = 0.25) +
        geom_text(data =countunique ,aes(label = round(count,2), x = mlong, y = mlat))+
        theme(aspect.ratio = 1) + labs(fill = "Cantidad")+
        scale_fill_gradient2( midpoint = mean(count),low = "red", mid = "white",
                              high = "blue") +
        theme_opts
      
    )
    
}

# Run the application 
shinyApp(ui = ui, server = server)

