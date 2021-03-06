mapa<- function(count) {
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
  ggplot() +
    geom_polygon(data = uystates_df, aes(x = long, y = lat, group = group, fill =
                                           count), color = "black", size = 0.25) +
    geom_text(data =countunique ,aes(label = round(count,2), x = mlong, y = mlat))+
    theme(aspect.ratio = 1) + labs(fill = "Siniestros/100hab")+
    scale_fill_gradient2( midpoint = mean(count),low = "blue", mid = "white",
                          high = "red") +
    ggtitle("Mapa de Uruguay") +
    theme_opts
}
