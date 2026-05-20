library(dplyr)
library(tidyverse)

load("data/Gobernadores_20211v.RData")
X2021_05_GobernadoresRegionales_Datos_Eleccion <- X2021_05_GobernadoresRegionales_Datos_Eleccion %>% 
  mutate(Nombre = paste(Nombres,`Primer apellido`,`Segundo apellido`))
df21 <- X2021_05_GobernadoresRegionales_Datos_Eleccion %>% 
  select("Distrito","Comuna","Local","Mesa","Lista","Pacto","Partido","Nombre","Votos")
lf21 <- df21 %>% group_by(Comuna,Local,Nombre,Pacto,Partido) %>% summarise(Resultado = sum(Votos))

X2024_11_GobernadoresRegionales_Datos_Eleccion_Araucania <- read_rds("~/R/electoral/data/Gobernador_2024.RData", refhook = NULL)
#load("data/Gobernador_2024.RData")
X2024_11_GobernadoresRegionales_Datos_Eleccion_Araucania <- X2024_11_GobernadoresRegionales_Datos_Eleccion_Araucania %>% 
  mutate(Nombre = paste(Nombres,`Primer apellido`,`Segundo apellido`))
df24 <- X2024_11_GobernadoresRegionales_Datos_Eleccion_Araucania %>% 
  select("Distrito","Comuna","Local","Mesa","Lista","Pacto","Partido","Nombre","Votos")
lf24 <- df24 %>% group_by(Comuna,Local,Nombre,Pacto,Partido) %>% summarise(Resultado = sum(Votos))









p <- X2024_11_GobernadoresRegionales_Datos_Eleccion_Araucania %>% 
  filter(Comuna %in% c("ANGOL")) %>% 
  select(Comuna, Local,  Votantes)
pc <- X2023_PConst_DV %>% 
  filter(NumRegion == 9 & Comuna %in% c("VILCUN")) %>% 
  select(Comuna, Sexo, Local, Mesa, Retario, Nacionalidad, Votantes)
p0 <- p %>% group_by(Comuna) %>% summarise(value = sum(Votantes))
ToT <- p0$value
p$P_Votantes <- signif((p$Votantes / ToT)*100,2) 
pc0 <- pc %>% group_by(Comuna) %>% summarise(value = sum(Votantes))