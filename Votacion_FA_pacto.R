library(dplyr)
library(tidyverse)
library(dslabs)
library(ggplot2)
library(ggrepel)
library(plyr)

#diputados 2017
vd17 <- read_rds("~/R/electoral/data/Diputados_2017.RData", refhook = NULL) %>% 
  mutate(Nombre = paste(Nombres,Papellido,Sapellido)) %>% 
  filter(N_Region == 9) %>% 
  select(N_Region, Distrito,Comuna,Lista,Pacto,Partido,Nvoto,Nombre,Votos)
vd17$Eleccion <- 2017
v17_pa <- ddply(vd17, .(Eleccion, Pacto), summarize, Votacion = sum(Votos))
v17_pa <- v17_pa %>% replace_na(list(Pacto = "BYN"))
v17_pa_val <- v17_pa %>% filter(Pacto != "BYN")
v17_pa$Perc <- round(prop.table(v17_pa$Votacion), 4)*100
v17_pa_val$Perc <- round(prop.table(v17_pa_val$Votacion), 4)*100
frente_a <- v17_pa_val %>% filter(Pacto == "FRENTE AMPLIO") %>% select(Eleccion,Pacto,Votacion,Perc)
otros_a <- v17_pa_val %>% filter(Pacto != "FRENTE AMPLIO") %>% select(Eleccion,Pacto,Votacion,Perc)
sd <- c("POR TODO CHILE","LA FUERZA DE LA MAYORIA","CONVERGENCIA DEMOCRATICA")
der <- c("SUMEMOS","CHILE VAMOS","INDEPENDIENTES")
sd_a <- v17_pa_val %>% filter(Pacto %in% sd) %>% 
  select(Eleccion,Pacto,Votacion,Perc)
der_a <- v17_pa %>% filter(Pacto %in% der) %>% 
  select(Eleccion,Pacto,Votacion,Perc)
byn_a <- v17_pa %>% filter(Pacto == "BYN") %>% select(Eleccion,Pacto,Votacion,Perc)
write_csv(v17_pa_val, file = "~/R/electoral/resultados/dip2017_ppacto.csv")

#diputados 2021
vd21 <- read_rds("~/R/electoral/data/Diputados_2021.RData", refhook = NULL) %>% 
  mutate(Nombre = paste(Nombres,Papellido,Sapellido)) %>% 
  filter(N_Region == 9) %>% 
  select(N_Region,Distrito,Comuna,Lista,Pacto,Partido,N_voto,Nombre,Votos)
vd21$Eleccion <- 2021
#vd21 <- vd21 %>% select(elecc,N_Region,Distrito,Comuna,Lista,Pacto,Partido,N_voto,Nombre,Votos)
vd21_pa <- ddply(vd21, .(Eleccion, Pacto), summarize, Votacion = sum(Votos))
vd21_pa <- vd21_pa %>% replace_na(list(Pacto = "BYN"))
vd21_pa_val <- vd21_pa %>% filter(Pacto != "BYN")
vd21_pa$Perc <- round(prop.table(vd21_pa$Votacion), 4)*100
vd21_pa_val$Perc <- round(prop.table(vd21_pa_val$Votacion), 4)*100
vd21_fa <- vd21_pa_val %>% filter(Pacto == "APRUEBO DIGNIDAD") %>% select(Eleccion,Pacto,Votacion,Perc)
vd21_ot <- vd21_pa_val %>% filter(Pacto != "APRUEBO DIGNIDAD") %>% select(Eleccion,Pacto,Votacion,Perc)
sd <- c("NUEVO PACTO SOCIAL","PARTIDO DE TRABAJADORES REVOLUCIONARIOS","PARTIDO ECOLOGISTA VERDE","PARTIDO PROGRESISTA DE CHILE")
der <- c("CHILE PODEMOS +","PARTIDO DE LA GENTE","INDEPENDIENTES UNIDOS","FRENTE SOCIAL CRISTIANO")
vd21_sd_a <- vd21_pa_val %>% filter(Pacto %in% sd) %>% 
  select(Eleccion,Pacto,Votacion,Perc)
vd21_der_a <- vd21_pa_val %>% filter(Pacto %in% der) %>% 
  select(Eleccion,Pacto,Votacion,Perc)
vd21_byn <- vd21_pa %>% filter(Pacto == "BYN") %>% select(Eleccion,Pacto,Votacion,Perc)
frente_a <- rbind(vd21_fa,frente_a)
otros_a <- rbind(vd21_ot, otros_a)
sd_a <- rbind(vd21_sd_a,sd_a)
der_a <- rbind(vd21_der_a,der_a)
byn_a <- rbind(vd21_byn, byn_a)
write_csv(vd21_pa_val, file = "~/R/electoral/resultados/dip2021_ppacto.csv")

#Concejeras/os 2023
vcc23 <- read_rds("~/R/electoral/data/2023_05_CCG.RData", refhook = NULL) %>% 
  filter(N_Region == 9) %>%
  select(Distrito,Comuna,Lista,Pacto,Partido,Nombre,Votos)
vcc23$Eleccion <- 2023
vcc23_pa <- ddply(vcc23, .(Eleccion, Pacto), summarize, Votacion = sum(Votos))
vcc23_pa <- vcc23_pa %>% replace_na(list(Pacto = "BYN"))
vcc23_pa_val <- vcc23_pa %>% filter(Pacto != "BYN")
vcc23_pa$Perc <- round(prop.table(vcc23_pa$Votacion), 4)*100
vcc23_pa_val$Perc <- round(prop.table(vcc23_pa_val$Votacion), 4)*100
#paso_val
paso_val <- vcc23_pa_val %>% filter(Pacto == "UNIDAD PARA CHILE") %>% select(Eleccion,Pacto,Votacion,Perc)
frente_a <- rbind(paso_val,frente_a)
vcc23_ot <- vcc23_pa_val %>% filter(Pacto != "UNIDAD PARA CHILE") %>% select(Eleccion,Pacto,Votacion,Perc)
der <- c("PARTIDO DE LA GENTE","PARTIDO REPUBLICANO DE CHILE","INDEPENDIENTES","CHILE SEGURO")
vcc23_sd_a <- vcc23_pa_val %>% filter(Pacto == "TODO POR CHILE") %>% 
  select(Eleccion,Pacto,Votacion,Perc)
vcc23_der_a <- vcc23_pa_val %>% filter(Pacto %in% der) %>% 
  select(Eleccion,Pacto,Votacion,Perc)
vcc23_byn <- vcc23_pa %>% filter(Pacto == "BYN") %>% select(Eleccion,Pacto,Votacion,Perc)
otros_a <- rbind(vcc23_ot, otros_a)
sd_a <- rbind(vcc23_sd_a,sd_a)
der_a <- rbind(vcc23_der_a,der_a)
byn_a <- rbind(vcc23_byn, byn_a)
write_csv(vcc23_pa_val, file = "~/R/electoral/resultados/cc2023_ppacto.csv")

#gobernadores 2024
vg24 <- read_rds("~/R/electoral/data/Gobernador_2024.RData", refhook = NULL) %>% 
  mutate(Nombre = paste(Nombres,`Primer apellido`,`Segundo apellido`)) %>% 
  select("Distrito","Comuna","Local","Mesa","Lista","Partido","Partido","Nombre","Votos") %>% 
  replace_na(list(Lista = "BYN"))
vg24$Eleccion <- 2024
#vg24 <- vg24 %>% 
#  select("elecc","Distrito","Comuna","Local","Mesa","Lista","Partido","Partido","Nombre","Votos")
vg24_li <- ddply(vg24, .(Eleccion, Lista), summarize, Votacion = sum(Votos))
names(vg24_li)[names(vg24_li) == 'Lista'] <- 'Pacto'
vg24_li_val <- vg24_li %>% filter(Pacto != "BYN") 
vg24_li_val$Perc <- round(prop.table(vg24_li_val$Votacion), 4)*100
vg24_li$Perc <- round(prop.table(vg24_li$Votacion), 4)*100
vg24_fa <- vg24_li_val %>% filter(Pacto == "O") %>% select(Eleccion,Pacto,Votacion,Perc) 
vg24_fa$Pacto <- replace(vg24_fa$Pacto, vg24_fa$Pacto == "O", "FRENTE AMPLIO")
vg24_ot <- vg24_li_val %>% filter(Pacto != "O") %>% select(Eleccion,Pacto,Votacion,Perc) 
der <- c("P","Z","F")
vg24_sd_a <- vg24_li_val %>% filter(Pacto == "ZZI") %>% 
  select(Eleccion,Pacto,Votacion,Perc)
vg24_der_a <- vg24_li_val %>% filter(Pacto %in% der) %>% 
  select(Eleccion,Pacto,Votacion,Perc)
vg24_byn <- vg24_li %>% filter(Pacto == "BYN") %>% select(Eleccion,Pacto,Votacion,Perc)
frente_a <- rbind(vg24_fa,frente_a)
otros_a <- rbind(vg24_ot, otros_a)
sd_a <- rbind(vg24_sd_a,sd_a)
der_a <- rbind(vg24_der_a,der_a)
byn_a <- rbind(vg24_byn, byn_a)
sd_a_pac <- ddply(sd_a, .(Eleccion), summarize, Votacion = sum(Votacion), Perc = sum(Perc) )
der_a_pac <- ddply(der_a, .(Eleccion), summarize, Votacion = sum(Votacion), Perc = sum(Perc) )
write_csv(vg24_li_val, file = "~/R/electoral/resultados/gob2024_ppacto.csv")

#resultados
frente_a$Pacto <- replace(frente_a$Pacto, frente_a$Pacto == "APRUEBO DIGNIDAD", "FRENTE AMPLIO")
write_csv(frente_a, file = "~/R/electoral/resultados/frente_a.csv")
write_csv(otros_a, file = "~/R/electoral/resultados/otros_a.csv")
write_csv(sd_a, file = "~/R/electoral/resultados/sd_a.csv")
write_csv(der_a, file = "~/R/electoral/resultados/der_a.csv")
write_csv(byn_a, file = "~/R/electoral/resultados/byn_a.csv")

fa <- ggplot(frente_a, aes(x = Eleccion)) +
  geom_line(aes(y = Votacion, colour = "Votación")) +
  geom_label_repel(aes(y = Votacion, label = Votacion), show.legend = FALSE) +
  geom_line(aes(y = Perc * 1000, colour = "Porcentaje")) +
  geom_label_repel(aes(y = Perc * 1000, label = round(Perc, 2)), show.legend = FALSE) +
  scale_y_continuous(
    name = "Votación",
    sec.axis = sec_axis(~./1000, name = "Porcentaje")
  ) +
  labs(
    x = "Elección",
    colour = "Variable"
  ) +
  theme_minimal() +
  theme(
    legend.position = c(0.6, 0.9),
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 10)
  ) +
  ggtitle("Votación FA - Araucanía 2017 -2024")
fa
#byn <- ggplot(byn_a, aes(x = elecc)) +
#  geom_line(aes(y = Perc, colour = "Porcentaje"), show.legend = FALSE) +
#  geom_label(aes(y = Perc, label = Perc)) +
#  ggtitle("Votación Blancos y Nulos 2017 -2024") 
#byn
sd <- ggplot(sd_a_pac, aes(x = Eleccion)) +
  geom_line(aes(y = Votacion, colour = "Votación")) +
  geom_label_repel(aes(y = Votacion, label = Votacion), show.legend = FALSE) +
  geom_line(aes(y = Perc * 1000, colour = "Porcentaje")) +
  geom_label_repel(aes(y = Perc * 1000, label = round(Perc, 2)), show.legend = FALSE) +
  scale_y_continuous(
    name = "Votación",
    sec.axis = sec_axis(~./1000, name = "Porcentaje")
  ) +
  labs(
    x = "Elección",
    colour = "Variable"
  ) +
  theme_minimal() +
  theme(
    legend.position = c(0.6, 0.9),
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 10)
  ) +
  ggtitle("Votación SD y Otros - Araucanía 2017 -2024")
sd
byn <- ggplot(byn_a, aes(x = Eleccion)) +
  geom_line(aes(y = Votacion, colour = "Votación")) +
  geom_label_repel(aes(y = Votacion, label = Votacion), show.legend = FALSE) +
  geom_line(aes(y = Perc * 1000, colour = "Porcentaje")) +
  geom_label_repel(aes(y = Perc * 1000, label = round(Perc, 2)), show.legend = FALSE) +
  scale_y_continuous(
    name = "Votación",
    sec.axis = sec_axis(~./1000, name = "Porcentaje")
  ) +
  labs(
    x = "Elección",
    colour = "Variable"
  ) +
  theme_minimal() +
  theme(
    legend.position = c(0.6, 0.9),
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 10)
  ) +
  ggtitle("Blacos y Nulos - Araucanía 2017 -2024")
byn
der <- ggplot(der_a_pac, aes(x = Eleccion)) +
  geom_line(aes(y = Votacion, colour = "Votación")) +
  geom_label_repel(aes(y = Votacion, label = Votacion), show.legend = FALSE) +
  geom_line(aes(y = Perc * 1000, colour = "Porcentaje")) +
  geom_label_repel(aes(y = Perc * 1000, label = round(Perc, 2)), show.legend = FALSE) +
  scale_y_continuous(
    name = "Votación",
    sec.axis = sec_axis(~./1000, name = "Porcentaje")
  ) +
  labs(
    x = "Elección",
    colour = "Variable"
  ) +
  theme_minimal() +
  theme(
    legend.position = c(0.6, 0.9),
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 10)
  ) +
  ggtitle("Votación Derecha - Araucanía 2017 -2024")
der
