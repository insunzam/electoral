library(dplyr)
library(tidyverse)
library(dslabs)
library(ggplot2)
library(gridExtra)

#diputados 2017
load("data/Diputados_2017.RData")
X2017_Diputados_Votacion <- X2017_Diputados_Votacion %>% 
  mutate(Nombre = paste(Nombres,Papellido,Sapellido))
vd17 <- X2017_Diputados_Votacion %>% filter(N_Region == 9)

vd17_p <- vd17 %>% group_by(Pacto) %>% summarize(VP = sum(Votos))
vd17_p <- vd17_p %>% replace_na(list(Pacto = "BYN"))
vd17_p$Perc <- round(prop.table(vd17_p$VP), 4)*100
#ggplot(vd17_p, aes(x = Pacto, colour = Pacto)) + geom_density()

p17 <- ggplot(vd17_p, aes(x = Pacto, y= Perc, fill = Pacto)) + 
  geom_bar(stat = "identity") +
  geom_text(aes(label = Perc), vjust = 0, colour = "black") +  
  coord_flip()
#p17
#diputados 2021
load("data/Diputados_2021.RData")
X2021_11_Diputados_Datos_Eleccion <- X2021_11_Diputados_Datos_Eleccion %>% 
  mutate(Nombre = paste(Nombres,Papellido,Sapellido))
vd21 <- X2021_11_Diputados_Datos_Eleccion %>% filter(N_Region == 9)

vd21_p <- vd21 %>% group_by(Pacto) %>% summarize(VP = sum(Votos))
vd21_p <- vd21_p %>% replace_na(list(Pacto = "BYN"))
vd21_p$Perc <- round(prop.table(vd21_p$VP), 4)*100

p21 <- ggplot(vd21_p, aes(x = Pacto, y= Perc, fill = Pacto)) + 
  geom_bar(stat = "identity") +
  geom_text(aes(label = Perc), vjust = 0, colour = "black") +  
  coord_flip()
#p21

#gobernadores 2024
load("data/Gobernadores_20241v.RData")
X2024_11_GobernadoresRegionales_Datos_Eleccion_Araucania <- X2024_11_GobernadoresRegionales_Datos_Eleccion_Araucania %>% 
  mutate(Nombre = paste(Nombres,`Primer apellido`,`Segundo apellido`))
df24 <- X2024_11_GobernadoresRegionales_Datos_Eleccion_Araucania %>% 
  select("Distrito","Comuna","Local","Mesa","Lista","Pacto","Partido","Nombre","Votos")
#lf24 <- df24 %>% group_by(Comuna,Local,Nombre,Pacto,Partido) %>% summarise(Resultado = sum(Votos))
lf24 <- df24 %>% group_by(Pacto) %>% summarise(Resultado = sum(Votos))
lf24 <- lf24 %>% replace_na(list(Pacto = "BYN"))
lf24$Perc <- round(prop.table(lf24$Resultado), 4)*100

p24 <- ggplot(lf24, aes(x = Pacto, y= Perc, fill = Pacto)) + 
  geom_bar(stat = "identity") +
  geom_text(aes(label = Perc), vjust = 0, colour = "black") +  
  coord_flip()
#p24

#grid.arrange(p17, p21, p24, ncol =2)
