library(dplyr)
library(tidyverse)
library(dslabs)
library(ggplot2)
library(ggrepel)
library(plyr)

#diputados 2017
X2017 <- read_rds("~/R/electoral/data/Diputados_2017.RData", refhook = NULL) %>% 
  mutate(Nombre = paste(Nombres,Papellido,Sapellido)) %>% 
  filter(N_Region == 9) %>% 
  select(N_Region, Distrito,Comuna,Lista,Pacto,Partido,Nvoto,Nombre,Votos)
X2017$Eleccion <- "Dip2017"

v17_pa <- ddply(X2017, .(Distrito,Eleccion, Partido), summarize, Votacion = sum(Votos))
v17_pa <- v17_pa %>% replace_na(list(Partido = "BYN"))
v17_pa_22 <- v17_pa %>% filter(Distrito == "DISTRITO 22")
v17_pa_23 <- v17_pa %>% filter(Distrito == "DISTRITO 23")
v17_pa_val_22 <- v17_pa_22 %>% filter(Partido != "BYN")
v17_pa_val_23 <- v17_pa_23 %>% filter(Partido != "BYN")
v17_pa_22$Perc <- round(prop.table(v17_pa_22$Votacion), 4)*100
v17_pa_23$Perc <- round(prop.table(v17_pa_23$Votacion), 4)*100
v17_pa_val_22$Perc <- round(prop.table(v17_pa_val_22$Votacion), 4)*100
v17_pa_val_23$Perc <- round(prop.table(v17_pa_val_23$Votacion), 4)*100

frente_a <- v17_pa_val_22 %>% filter(Partido == "PARTIDO HUMANISTA") %>% select(Eleccion,Distrito,Partido,Votacion,Perc)
paso_val <- v17_pa_val_23 %>% filter(Partido == "PARTIDO HUMANISTA") %>% select(Eleccion,Distrito,Partido,Votacion,Perc)
frente_a <- rbind(paso_val,frente_a)
sd <- c("PARTIDO DEMOCRATA CRISTIANO","PARTIDO POR LA DEMOCRACIA","PARTIDO SOCIALISTA DE CHILE",
        "PARTIDO RADICAL SOCIALDEMOCRATA")
der <- c("RENOVACION NACIONAL","UNION DEMOCRATA INDEPENDIENTE","AMPLITUD","EVOLUCION POLITICA","INDEPENDIENTES")
izq <- c("PARTIDO COMUNISTA DE CHILE","PAIS","PARTIDO PROGRESISTA")
sd_a <- v17_pa_val_22 %>% filter(Partido %in% sd) %>% select(Eleccion,Distrito,Partido,Votacion,Perc)
paso_val <- v17_pa_val_23 %>% filter(Partido %in% sd) %>% select(Eleccion,Distrito,Partido,Votacion,Perc)
sd_a <- rbind(paso_val,sd_a)
der_a <- v17_pa_val_22 %>% filter(Partido %in% der) %>% select(Eleccion,Distrito,Partido,Votacion,Perc)
paso_val <- v17_pa_val_23 %>% filter(Partido %in% der) %>% select(Eleccion,Distrito,Partido,Votacion,Perc)
der_a <- rbind(paso_val,der_a)
izq_a <- v17_pa_val_22 %>% filter(Partido %in% izq) %>% select(Eleccion,Distrito,Partido,Votacion,Perc)
paso_val <- v17_pa_val_23 %>% filter(Partido %in% izq) %>% select(Eleccion,Distrito,Partido,Votacion,Perc)
izq_a <- rbind(paso_val,izq_a)

#diputados 2021
X2021<- read_rds("~/R/electoral/data/Diputados_2021.RData", refhook = NULL) %>% 
  mutate(Nombre = paste(Nombres,Papellido,Sapellido)) %>% 
  filter(N_Region == 9) %>% 
  select(N_Region,Distrito,Comuna,Lista,Pacto,Partido,N_voto,Nombre,Votos)
X2021$Eleccion <- "Dip2021"
X2021$Partido <- replace(X2021$Partido, X2021$Partido == "INDEPENDIENTE EVOLUCION POLITICA", "EVOLUCION POLITICA")
X2021$Partido <- replace(X2021$Partido, X2021$Partido == "INDEPENDIENTE FEDERACION REGIONALISTA VERDE SOCIAL", "FEDERACION REGIONALISTA VERDE SOCIAL")
X2021$Partido <- replace(X2021$Partido, X2021$Partido == "INDEPENDIENTE PARTIDO COMUNISTA DE CHILE", "PARTIDO COMUNISTA DE CHILE")
X2021$Partido <- replace(X2021$Partido, X2021$Partido == "INDEPENDIENTE PARTIDO CONSERVADOR CRISTIANO", "PARTIDO CONSERVADOR CRISTIANO")
X2021$Partido <- replace(X2021$Partido, X2021$Partido == "INDEPENDIENTE PARTIDO HUMANISTA", "PARTIDO HUMANISTA")
X2021$Partido <- replace(X2021$Partido, X2021$Partido == "INDEPENDIENTE PARTIDO NACIONAL CIUDADANO", "PARTIDO NACIONAL CIUDADANO")
X2021$Partido <- replace(X2021$Partido, X2021$Partido == "INDEPENDIENTE PARTIDO REPUBLICANO DE CHILE", "PARTIDO REPUBLICANO DE CHILE")
X2021$Partido <- replace(X2021$Partido, X2021$Partido == "INDEPENDIENTE REVOLUCION DEMOCRATICA", "REVOLUCION DEMOCRATICA")
X2021$Partido <- replace(X2021$Partido, X2021$Partido == "INDEPENDIENTE UNION DEMOCRATA INDEPENDIENTE", "UNION DEMOCRATA INDEPENDIENTE")
X2021$Partido <- replace(X2021$Partido, X2021$Partido == "INDEPENDIENTE CENTRO UNIDO", "CENTRO UNIDO")
X2021$Partido <- replace(X2021$Partido, X2021$Partido == "INDEPENDIENTE IGUALDAD", "PARTIDO IGUALDAD")
X2021$Partido <- replace(X2021$Partido, X2021$Partido == "INDEPENDIENTE PARTIDO RADICAL DE CHILE", "PARTIDO RADICAL DE CHILE")

#vd21_p <- vd21 %>% group_by(elecc,Partido) %>% summarize(VP = sum(Votos))
#vd21_p <- vd21_p %>% replace_na(list(Partido = "BYN"))
#vd21_p$Perc <- round(prop.table(vd21_p$VP), 4)*100
#vd21_perc <- vd21_p %>% select(elecc, Partido, Perc)
#acum_votos <- rbind(vd21_p,acum_votos)
#vd21_p$Perc <- round(prop.table(vd21_p$VP), 4)*100
#acum_perc <- rbind(vd21_perc,acum_perc)

vd21_pa <- ddply(X2021, .(Distrito,Eleccion, Partido), summarize, Votacion = sum(Votos))
vd21_pa <- vd21_pa %>% replace_na(list(Partido = "BYN"))
vd21_pa_22 <- vd21_pa %>% filter(Distrito == "DISTRITO 22")
vd21_pa_23 <- vd21_pa %>% filter(Distrito == "DISTRITO 23")
vd21_pa_22_val <- vd21_pa_22 %>% filter(Partido != "BYN")
vd21_pa_23_val <- vd21_pa_23 %>% filter(Partido != "BYN")
vd21_pa_22$Perc <- round(prop.table(vd21_pa_22$Votacion), 4)*100
vd21_pa_23$Perc <- round(prop.table(vd21_pa_23$Votacion), 4)*100
vd21_pa_22_val$Perc <- round(prop.table(vd21_pa_22_val$Votacion), 4)*100
vd21_pa_23_val$Perc <- round(prop.table(vd21_pa_23_val$Votacion), 4)*100
paso_val <- vd21_pa_22_val %>% filter(Partido == "REVOLUCION DEMOCRATICA") %>% select(Eleccion,Distrito,Partido,Votacion,Perc)
frente_a <- rbind(paso_val,frente_a)
paso_val <- vd21_pa_23_val %>% filter(Partido == "REVOLUCION DEMOCRATICA") %>% select(Eleccion,Distrito,Partido,Votacion,Perc)
frente_a <- rbind(paso_val,frente_a)

#Concejeras/os 2023
X2023 <- read_rds("~/R/electoral/data/2023_05_CCG.RData", refhook = NULL) %>% 
  filter(N_Region == 9) %>%
  select(Distrito,Comuna,Lista,Pacto,Partido,Nombre,Votos)
X2023$Eleccion <- "CC2023"
X2023$Partido <- replace(X2023$Partido, X2023$Partido == "INDEPENDIENTE EVOLUCION POLITICA", "EVOLUCION POLITICA")
X2023$Partido <- replace(X2023$Partido, X2023$Partido == "INDEPENDIENTE PARTIDO COMUNISTA DE CHILE", "PARTIDO COMUNISTA DE CHILE")
X2023$Partido <- replace(X2023$Partido, X2023$Partido == "INDEPENDIENTE UNION DEMOCRATA INDEPENDIENTE", "UNION DEMOCRATA INDEPENDIENTE")
X2023$Partido <- replace(X2023$Partido, X2023$Partido == "INDEPENDIENTE PARTIDO RADICAL DE CHILE", "PARTIDO RADICAL DE CHILE")
X2023$Partido <- replace(X2023$Partido, X2023$Partido == "INDEPENDIENTE PARTIDO POR LA DEMOCRACIA", "PARTIDO POR LA DEMOCRACIA")
X2023$Partido <- replace(X2023$Partido, X2023$Partido == "INDEPENDIENTE RENOVACION NACIONAL", "RENOVACION NACIONAL")
vcc23_pa <- ddply(X2023, .(Distrito,Eleccion, Partido), summarize, Votacion = sum(Votos))
paso_vcc23 <- X2023
paso_vcc23$Partido <- replace(paso_vcc23$Partido, paso_vcc23$Partido == "REVOLUCION DEMOCRATICA", "FA")
paso_vcc23$Partido <- replace(paso_vcc23$Partido, paso_vcc23$Partido == "CONVERGENCIA SOCIAL", "FA")
vcc23_com <- ddply(paso_vcc23, .(Comuna, Partido), summarize, Votacion = sum(Votos))
vcc23_com <- vcc23_com %>% replace_na(list(Partido = "BYN"))
vcc23_com$Perc <- round(prop.table(vcc23_com$Votacion), 4)*100
frente_a_com <- vcc23_com %>% filter(Partido == "FA") %>% select(Comuna,Partido,Votacion,Perc)

vcc23_pa <- vcc23_pa %>% replace_na(list(Partido = "BYN"))
vcc23_pa_22 <- vcc23_pa %>% filter(Distrito == "DISTRITO 22")
vcc23_pa_23 <- vcc23_pa %>% filter(Distrito == "DISTRITO 23")
vcc23_pa_22_val <- vcc23_pa_22 %>% filter(Partido != "BYN")
vcc23_pa_23_val <- vcc23_pa_23 %>% filter(Partido != "BYN")
vcc23_pa_22$Perc <- round(prop.table(vcc23_pa_22$Votacion), 4)*100
vcc23_pa_23$Perc <- round(prop.table(vcc23_pa_23$Votacion), 4)*100
vcc23_pa_22_val$Perc <- round(prop.table(vcc23_pa_22_val$Votacion), 4)*100
vcc23_pa_23_val$Perc <- round(prop.table(vcc23_pa_23_val$Votacion), 4)*100
fa <- c("REVOLUCION DEMOCRATICA","CONVERGENCIA SOCIAL")
#paso_val
paso_val <- vcc23_pa_22_val %>% filter(Partido %in% fa) %>% select(Eleccion,Distrito,Partido,Votacion,Perc)
frente_a <- rbind(paso_val,frente_a)
paso_val <- vcc23_pa_23_val %>% filter(Partido %in% fa) %>% select(Eleccion,Distrito,Partido,Votacion,Perc)
frente_a <- rbind(paso_val,frente_a)

#gobernadores 2024
X2024<- read_rds("~/R/electoral/data/Gobernador_2024.RData", refhook = NULL) %>% 
  mutate(Nombre = paste(Nombres,`Primer apellido`,`Segundo apellido`)) %>%
  select(Distrito,Comuna,Lista,Pacto,Partido,Nombre,Votos)
X2024$Eleccion <- "Gob2024"
vg24 <- ddply(X2024, .(Distrito,Eleccion, Partido), summarize, Votacion = sum(Votos))
#  X2024 %>% group_by(Distrito,Eleccion,Pacto) %>% summarise(Votacion = sum(Votos))
vg24 <- vg24 %>% replace_na(list(Pacto = "BYN"))
vg24_22 <- vg24 %>% filter(Distrito == "DISTRITO 22")
vg24_23 <- vg24 %>% filter(Distrito == "DISTRITO 23")
vg_24_22_val <- vg24_22 %>% filter(Pacto != "BYN")
vg_24_23_val <- vg24_23 %>% filter(Pacto != "BYN")
vg24_22$Perc <- round(prop.table(vg24_22$Votacion), 4)*100
vg24_23$Perc <- round(prop.table(vg24_23$Votacion), 4)*100
vg_24_22_val$Perc <- round(prop.table(vg_24_22_val$Votacion), 4)*100
vg_24_23_val$Perc <- round(prop.table(vg_24_23_val$Votacion), 4)*100
names(vg_24_22_val)[names(vg_24_22_val) == 'Pacto'] <- 'Partido'
names(vg_24_23_val)[names(vg_24_23_val) == 'Pacto'] <- 'Partido'
paso_val <- vg_24_22_val %>% filter(Partido == "POR CHILE Y SUS REGIONES") %>% select(Eleccion,Distrito,Partido,Votacion,Perc)
frente_a <- rbind(paso_val,frente_a)
paso_val <- vg_24_23_val %>% filter(Partido == "POR CHILE Y SUS REGIONES") %>% select(Eleccion,Distrito,Partido,Votacion,Perc)
frente_a <- rbind(paso_val,frente_a)
frente_a$Partido <- replace(frente_a$Partido, frente_a$Partido == "POR CHILE Y SUS REGIONES", "FRENTE AMPLIO")
write_csv(frente_a, file = "~/R/electoral/resultados/frente_a_partido.csv")

l <- ggplot(frente_a, aes(Distrito, Votacion))
l + geom_tile(aes(fill = Eleccion))


