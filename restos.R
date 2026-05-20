year <- c("2010","2011","2012","2013","2014","2015","2016","2017","2018","2019","2020","2010","2011","2012","2013","2014","2015","2016","2017","2018","2019","2020")
sex <- c("male","male","male","male","male","male","male","male","male","male","male","female","female","female","female","female","female","female","female","female","female","female")
number <- c(12,5,15,5,4,6,5,5,8,4,0,54,64,70,50,54,35,40,37,42,36,24)
perFailed <- c(0.090909091,0.057971014,0.117647059,0.163636364,0.068965517,0.048780488,0.044444444,0.071428571, 0.1,0.1,0)
data <- data.frame(year,sex,number,perFailed)
data$year <- as.factor(data$year)

ggplot(data=data, aes(x=year, y=number, fill=sex)) +
  geom_bar(stat="identity", position=position_dodge()) +
  theme_bw()


ratio <- max(data$number) / max(data$perFailed)
data <- transform(data, perFailedScaled = perFailed * ratio)
head(data)

ggplot(data=data, aes(x=year, y=number, fill=sex)) +
  geom_bar(stat="identity", position=position_dodge()) +
  geom_line(aes(x = match(year, sort(unique(year))), y = perFailedScaled), 
            data = ~ subset(., sex == "male")) +
  scale_y_continuous(sec.axis = sec_axis(~ . / ratio, labels = scales::label_percent())) +
  theme_bw()



+
  scale_x_discrete(
    "Eleccion",
    labels = c(
      1 = "Pres 17",
      2 = "Pres 21",
      3 = "PlebSal 22",
      4 = "PlebSal 22",
      5 = "Gob 24"
    )) 


mapa_comunas_filtro %>% 
  st_set_geometry(mapa_comunas_filtro$geometry) %>%
  ggplot() +
  aes(fill = poblacion) +
  geom_sf(linewidth = 0.12, color = "white") +
  geom_sf_text(aes(label = comma(poblacion, big.mark = ".")), 
               size = 2, color = "white", check_overlap = T) +
  theme_classic() +
  scale_fill_distiller(type = "seq", palette = 12,
                       labels = label_comma(big.mark = ".")) + 
  theme(legend.key.width = unit(3, "mm")) +
  theme(axis.title = element_blank())


#https://bastianolea.rbind.io/blog/tutorial_mapa_chile/

library(ggplot2)
library(maps)
world <- map_data("world")
ggplot(data = world, aes(x = long, y = lat, group = group)) + 
  geom_polygon() 


ggplot(world, aes(map_id = region)) +
  geom_map(data = world, map = world,
           aes(x = long, y = lat, map_id = region))



mapa_zona_09 <- st_as_sf(mapa_zona_09)
grafico_comunas <- mapa_zona_09 |> 
  st_set_geometry(mapa_zona_09$geometry) |> # asignar geometría
  ggplot() + # gráfico
  geom_sf() # capa geométrica

grafico_comunas +
  theme_classic()


mapa_zona_09 <- st_as_sf(mapa_zona_09)
grafico_comunas <- mapa_zona_09 |> 
  st_set_geometry(mapa_zona_09$geometry) |> # asignar geometría
  ggplot() + # gráfico
  geom_sf() # capa geométrica

grafico_comunas +
  theme_classic()