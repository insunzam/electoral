library(dplyr)
library(tidyverse)
library(dslabs)
library(gridExtra)
library(sqldf)
library(reshape2)

#setwd("/home/insunzam/R/electoral")
#Carga datos
load("data/CR_2021_DV.RData")
load("data/PCon_2023_DV.RData")
load("data/terr_elec.RData")
p <- X2021_11_CR_DE_DV %>% 
  filter(NumRegion == 9 & Comuna %in% c("VILCUN")) %>% 
  select(Comuna, Sexo, Mesa, RangoEtario, Nacionalidad, Votantes)
pc <- X2023_PConst_DV %>% 
  filter(NumRegion == 9 & Comuna %in% c("VILCUN")) %>% 
  select(Comuna, Sexo, Mesa, Retario, Nacionalidad, Votantes)
p0 <- p %>% group_by(Comuna) %>% summarise(value = sum(Votantes))
ToT <- p0$value
p$P_Votantes <- signif((p$Votantes / ToT)*100,2) 
pc0 <- pc %>% group_by(Comuna) %>% summarise(value = sum(Votantes))
ToT_1 <- pc0$value
pc$P_Votantes <- signif((pc$Votantes / ToT_1)*100,2)
te <- Territorios_Electorales %>% rename("CIRCSEN" = "Circunscripción Senatorial", 
                                         "CIRCPROV" = "Circunscripción Provincial", 
                                         "DISTRITO" = "Distrito")
te$Comuna <- chartr("ÁÉÍÓÚ", "AEIOU", toupper(te$Comuna))
te$CIRCPROV <- toupper(te$CIRCPROV)
p <- inner_join(p,te, by="Comuna")
pc <- inner_join(pc,te, by="Comuna")

#Datos por Sexo
p1 <- p %>% group_by(Comuna, Sexo) %>% summarise(value = sum(P_Votantes))
p4 <- dcast(p1, Comuna ~ Sexo)
p4 <- p4 %>% rename("VH2021" = "HOMBRES", "VM2021" = "MUJERES") 
pc1 <- pc %>% group_by(Comuna, Sexo) %>% summarise(V2023 = sum(P_Votantes))
pc4 <- dcast(pc1, Comuna ~ Sexo)
pc4 <- pc4 %>% rename("VH2023" = "HOMBRES", "VM2023" = "MUJERES") 
CompSex <- inner_join(p4, pc4, by="Comuna")
CompSex <- melt(CompSex, id.vars = "Comuna") 



g <- ggplot(CompSex, aes(fill=variable, y=value, x=Comuna)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) 
#+ 
#  theme_void() + 
#  theme(legend.position="dodge")
g +geom_text(aes(label = value), position = position_dodge(0.9), 
             vjust = 0, color = "white", size=6) +
  scale_fill_brewer(palette="Set1")

#g + geom_text(aes(label = value), 
#              position = position_dodge(0.9),
#              vjust = 0)



g <- ggplot(CompSex, aes(fill=variable, y=value, x=Comuna)) +
  geom_bar(stat = "identity", position = 'dodge') 
g + geom_text(aes(label = value), 
            position = position_dodge(0.9),
            vjust = 0)

p2 <- p %>% group_by(Comuna, RangoEtario) %>% summarise(value = sum(P_Votantes))
p3 <- dcast(p2, Comuna ~ RangoEtario)
pc2 <- pc %>% group_by(Comuna, Retario) %>% summarise(V2023 = sum(P_Votantes))
pc3 <- dcast(pc2, Comuna ~ Retario)
CompSex <- inner_join(p3, pc3, by="Comuna")
CompSex <- melt(CompSex, id.vars = "Comuna") 
g <- ggplot(CompSex, aes(fill=variable, y=value, x=Comuna)) + 
              geom_bar(stat = "identity", position = 'dodge')
g + geom_text(aes(label = value), 
              position = position_dodge(0.9),
              vjust = 0)

