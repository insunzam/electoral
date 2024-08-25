library(dplyr)
library(tidyverse)
library(dslabs)
library(gridExtra)
library(sqldf)
library(reshape2)

#setwd("/home/insunzam/R/electoral")
load("data/CR_2021_DV.RData")
load("data/PCon_2023_DV.RData")
load("data/terr_elec.RData")

# Carga fuentes
p <- X2021_11_CR_DE_DV %>% 
  filter(NumRegion == 9 & Comuna %in% c("TEMUCO", "PADRE LAS CASAS")) %>% 
  select(Comuna, Sexo, Mesa, RangoEtario, Nacionalidad, Votantes)
pc <- X2023_PConst_DV %>% 
  filter(NumRegion == 9 & Comuna %in% c("TEMUCO", "PADRE LAS CASAS")) %>% 
  select(Comuna, Sexo, Mesa, Retario, Nacionalidad, Votantes)
#Carga totales por comuna
p0 <- p %>% group_by(Comuna) %>% summarise(value = sum(Votantes))
pc0 <- p %>% group_by(Comuna) %>% summarise(value = sum(Votantes))

#ToT_h <- p$Sexo == "HOMBRES"

p1 <- p %>% group_by(Comuna, Sexo) %>% summarise(value = sum(Votantes))
p4 <- dcast(p1, Comuna ~ Sexo)
p4_2 <- p4 %>% rename("VH2021" = "HOMBRES", "VM2021" = "MUJERES") 
pc1 <- pc %>% group_by(Comuna, Sexo) %>% summarise(V2023 = sum(Votantes))
pc4 <- dcast(pc1, Comuna ~ Sexo)
pc4_2 <- pc4 %>% rename("VH2023" = "HOMBRES", "VM2023" = "MUJERES") 
CompSex <- inner_join(p4_2, pc4_2, by="Comuna")
CompSex <- melt(CompSex, id.vars = "Comuna") 
g <- ggplot(CompSex, aes(fill=variable, y=value, x=Comuna)) +
  geom_bar(stat = "identity", position = 'dodge') 
g + geom_text(aes(label = value), 
            position = position_dodge(0.9),
            vjust = 0)

p2 <- p %>% group_by(Comuna, RangoEtario) %>% summarise(value = sum(Votantes))
p3 <- dcast(p2, Comuna ~ RangoEtario)
#p3_2 <- p4 %>% rename("VH2021" = "HOMBRES", "VM2021" = "MUJERES") 
pc2 <- pc %>% group_by(Comuna, Retario) %>% summarise(V2023 = sum(Votantes))
pc3 <- dcast(pc2, Comuna ~ Retario)
#pc3_2 <- pc4 %>% rename("VH2023" = "HOMBRES", "VM2023" = "MUJERES") 
CompSex <- inner_join(p3, pc3, by="Comuna")
CompSex <- melt(CompSex, id.vars = "Comuna") 
g <- ggplot(CompSex, aes(fill=variable, y=value, x=Comuna)) + 
              geom_bar(stat = "identity", position = 'dodge')
g + geom_text(aes(label = value), 
              position = position_dodge(0.9),
              vjust = 0)
