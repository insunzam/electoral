library(dplyr)
library(tidyverse)
library(dslabs)
library(ggplot2)
library(ggrepel)
library(plyr)

#Fuentes participacion
load("data/part_pres_2017.RData")
load("data/part_pres_2021.RData")
load("data/part_pc_2022.RData")
load("data/part_pc_2023.RData")
load("data/part_gr_2024.RData")
terr <- read_rds("~/R/electoral/data/terr_elec.RData", refhook = NULL) %>% 
  filter(Circ_Senatorial == 11) %>% 
  select(Comuna,Cod_Comuna)
#  load("data/terr_elec.RData")
names(part_pres_2017)[names(part_pres_2017) == 'Nro.Región'] <- 'N_region'
names(part_pres_2017)[names(part_pres_2017) == 'Circunscripción electoral'] <- 'CIRCELEC'
names(part_pres_2017)[names(part_pres_2017) == 'Votación'] <- 'Votacion'
par17 <- part_pres_2017 %>% filter(N_region == 9)
par17$elecc <- 1
names(participacion_Pres_2021)[names(participacion_Pres_2021) == 'Nro.Región'] <- 'N_region'
names(participacion_Pres_2021)[names(participacion_Pres_2021) == 'Circunscripción electoral'] <- 'CIRCELEC'
names(participacion_Pres_2021)[names(participacion_Pres_2021) == 'Votación'] <- 'Votacion'
par21 <- participacion_Pres_2021 %>% filter(N_region == 9)
par21$elecc <- 2
par22 <- X2022_PC_Datos  %>% filter(N_region == 9)
par22$elecc <- 3
names(participacion_PC2023)[names(participacion_PC2023) == 'Nro.Región'] <- 'N_region'
names(participacion_PC2023)[names(participacion_PC2023) == 'Circunscripción electoral'] <- 'CIRCELEC'
par23 <- participacion_PC2023 %>% filter(N_region == 9)
par23$elecc <- 4
names(participacion_GR_2024)[names(participacion_GR_2024) == 'Nro.Región'] <- 'N_region'
names(participacion_GR_2024)[names(participacion_GR_2024) == 'Circunscripción electoral'] <- 'CIRCELEC'
par24 <- participacion_GR_2024 %>% filter(N_region == 9)
par24$elecc <- 5

participa_tot <- ddply(par17, .(Comuna, elecc), summarize, Voto = sum(Votacion), Insc = sum(Inscritos))
par17_comuna <- participa_tot %>% left_join(terr, by = join_by(Comuna))
par21 <- ddply(par21, .(Comuna, elecc), summarize, Voto = sum(Votacion), Insc = sum(Inscritos))
#par21_comuna <- par21 %>% left_join(terr, by = join_by(Comuna))
#write_rds(par21_comuna,"~/R/electoral/data/par_21.RData")
participa_tot <- rbind(par21,participa_tot)
par22 <- ddply(par22, .(Comuna, elecc), summarize, Voto = sum(Votacion), Insc = sum(Inscritos))
participa_tot <- rbind(par22,participa_tot)
par23 <- ddply(par23, .(Comuna, elecc), summarize, Voto = sum(Votantes), Insc = sum(Inscritos))
participa_tot <- rbind(par23,participa_tot)
par24 <- ddply(par24, .(Comuna, elecc), summarize, Voto = sum(Votos), Insc = sum(Inscritos))
participa_tot <- rbind(par24,participa_tot)
participa_tot$Participacion <- signif((participa_tot$Voto / participa_tot$Insc)*100,4)
write_csv(participa_tot, file = "~/R/electoral/resultados/participacion_tot.csv")
participa_tot_elecc <- ddply(participa_tot, .(elecc), summarize, Voto = sum(Voto), Insc = sum(Insc))
participa_tot_elecc$Part <- signif((participa_tot_elecc$Voto / participa_tot_elecc$Insc)*100,4)
write_csv(participa_tot_elecc, file = "~/R/electoral/resultados/participacion_17-24.csv")

#Resultados
part <- ggplot(participa_tot_elecc, aes(x = elecc)) +
  geom_line(aes(y = Voto, colour = "Votación")) +
  geom_label_repel(aes(y = Voto, label = Voto), show.legend = FALSE) +
#  ylim(0,1000) +
  geom_line(aes(y = Part * 1000, colour = "Porcentaje")) +
  geom_label_repel(aes(y = Part * 1000, label = round(Part, 2)), show.legend = FALSE) +
  scale_y_continuous(
    name = "Votación",
    sec.axis = sec_axis(~./100000, labels = scales::label_percent(), name = "Porcentaje")
  ) +
  labs(
    x = "Elección",
    colour = "Variable"
  ) +
  theme_minimal() +
  theme(
    legend.position = c(0.5, 0.9),
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 10)
  ) +
  ggtitle("Participación - Araucanía 2017 -2024")
part



#p <- ggplot(participa_tot_elecc, aes(x = elecc)) +
#  geom_line(aes(y = Part, colour = "Porcentaje"), show.legend = FALSE) +
#  geom_label(aes(y = Part, label = Part)) +
#  ggtitle("Participación Electoral Araucanía 2017 -2024")
#p

