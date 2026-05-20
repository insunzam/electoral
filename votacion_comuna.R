library(dplyr)
library(tidyverse)
library(dslabs)
library(ggplot2)
library(ggrepel)
library(plyr)

X2024<- read_rds("~/R/electoral/data/Gobernador_2024.RData", refhook = NULL) %>% 
  mutate(Nombre = paste(Nombres,`Primer apellido`,`Segundo apellido`)) %>%
  select(Distrito,Comuna,Lista,Pacto,Partido,Nombre,Votos)
#X2024$Eleccion <- "Gob2024"
vg24_com <- ddply(X2024, .(Comuna,Nombre), summarize, Votacion = sum(Votos))
byn <- c("VOTOS EN BLANCO NA NA","VOTOS NULOS NA NA") 
#tab <- xtabs(Votacion ~ Comuna + Nombre, data = vg24_com )
vg24_com$Perc <- round(prop.table(vg24_com$Votacion,margin = NULL), 4)*100
vg24_com_val <- subset(vg24_com, !(Nombre %in% byn))
vg24_com_lp <- subset(vg24_com, Nombre == "LUIS ALBERTO PENCHULEO MORALES")
write_csv(vg24_com_lp, file = "~/R/electoral/resultados/lp_2024.csv")
write_rds(vg24_com_lp, file = "~/R/electoral/data/lp_2024.RData")

#frente_a$Partido <- replace(frente_a$Partido, frente_a$Partido == "POR CHILE Y SUS REGIONES", "FRENTE AMPLIO")
#write_csv(frente_a, file = "~/R/electoral/resultados/frente_a_partido.csv")