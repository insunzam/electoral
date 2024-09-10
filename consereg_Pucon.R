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
  filter(NumRegion == 9 & Comuna %in% c("PUCON")) %>% 
  select(Comuna, Sexo, Local, Mesa, RangoEtario, Nacionalidad, Votantes)
pc <- X2023_PConst_DV %>% 
  filter(NumRegion == 9 & Comuna %in% c("PUCON")) %>% 
  select(Comuna, Sexo, Local, Mesa, Retario, Nacionalidad, Votantes)
p0 <- p %>% group_by(Comuna) %>% summarise(value = sum(Votantes))
p$P_Votantes <- signif((p$Votantes/p0$value)*100,digits=4) 
pc0 <- pc %>% group_by(Comuna) %>% summarise(value = sum(Votantes))
pc$P_Votantes <- signif((pc$Votantes/pc0$value)*100,digits=4)
te <- Territorios_Electorales %>% rename("CIRCSEN" = "Circunscripción Senatorial", 
                                         "CIRCPROV" = "Circunscripción Provincial", 
                                         "DISTRITO" = "Distrito")
te$Comuna <- chartr("ÁÉÍÓÚ", "AEIOU", toupper(te$Comuna))
te$CIRCPROV <- toupper(te$CIRCPROV)
p <- inner_join(p,te, by="Comuna")
pc <- inner_join(pc,te, by="Comuna")

#Datos por Sexo
p1 <- p %>% group_by(Comuna, Sexo) %>% summarise(value = trunc(sum(P_Votantes),2))
p4 <- dcast(p1, Comuna ~ Sexo)
p4 <- p4 %>% rename("VH2021" = "HOMBRES", "VM2021" = "MUJERES") 
pc1 <- pc %>% group_by(Comuna, Sexo) %>% summarise(V2023 = trunc(sum(P_Votantes),2))
pc4 <- dcast(pc1, Comuna ~ Sexo)
pc4 <- pc4 %>% rename("VH2023" = "HOMBRES", "VM2023" = "MUJERES")
pls <- p %>% group_by(Comuna, Local, Sexo) %>% summarise(value = sum(Votantes))
plre <- p %>% group_by(Comuna, Local, RangoEtario) %>% summarise(value = sum(Votantes))
pls_23 <- pc %>% group_by(Comuna, Local, Sexo) %>% summarise(value = sum(Votantes))
plre_23 <- pc %>% group_by(Comuna, Local, Retario) %>% summarise(value = sum(Votantes))

g21 <- ggplot(p1, aes(fill=Sexo, y=value, x=Comuna)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) + 
  geom_text(aes(label = value), position = position_stack(vjust = 0.5),
            vjust = 0, color = "white", size=4) +
  ggtitle("Porcentaje de Votantes por Sexo 
          Pucón 2021 - Consejeros Regionales")
g23 <- ggplot(pc1, aes(fill=Sexo, y=V2023, x=Comuna)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) + 
  geom_text(aes(label = V2023), position = position_stack(vjust = 0.5),
            vjust = 0, color = "white", size=4) +
  ggtitle("Porcentaje de Votantes por Sexo 
          Pucón 2023 - Plebiscito Constitucional")
grid.arrange(g21, g23)
p1_1 <- p %>% group_by(Comuna, Sexo) %>% summarise(value = trunc(sum(Votantes),2))
p4_1 <- dcast(p1_1, Comuna ~ Sexo)
p4_1 <- p4_1 %>% rename("VH2021" = "HOMBRES", "VM2021" = "MUJERES") 
pc1_1 <- pc %>% group_by(Comuna, Sexo) %>% summarise(V2023 = trunc(sum(Votantes),2))
pc4_1 <- dcast(pc1_1, Comuna ~ Sexo)
pc4_1 <- pc4_1 %>% rename("VH2023" = "HOMBRES", "VM2023" = "MUJERES") 
CompSex <- inner_join(p4_1, pc4_1, by="Comuna")
CompSex <- melt(CompSex, id.vars = "Comuna") 

g <- ggplot(CompSex, aes(fill=variable, y=value, x=Comuna)) +
  geom_bar(stat = "identity", position = 'dodge') 
g + geom_text(aes(label = value), position = position_dodge(0.9),
              vjust = 0, color = "blue", size=4) +
  ggtitle("Total Comparado de Votantes Pucón por Sexo - 2021 - 2023")
  
p2 <- p %>% group_by(Comuna, RangoEtario) %>% summarise(value = trunc(sum(P_Votantes),2))
p3 <- dcast(p2, Comuna ~ RangoEtario)
pc2 <- pc %>% group_by(Comuna, Retario) %>% summarise(V2023 = trunc(sum(P_Votantes),2))
pc3 <- dcast(pc2, Comuna ~ Retario)
g21 <- ggplot(p2, aes(fill=RangoEtario, y=value, x=Comuna)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) + 
  geom_text(aes(label = value), position = position_stack(vjust = 0.5),
            vjust = 0, color = "white", size=4) +
  ggtitle("Porcentaje de Votantes por Edad 
          Pucón 2021 - Consejeros Regionales")
g23 <- ggplot(pc2, aes(fill=Retario, y=V2023, x=Comuna)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) + 
  geom_text(aes(label = V2023), position = position_stack(vjust = 0.5),
            vjust = 0, color = "white", size=4) +
  ggtitle("Porcentaje de Votantes por Edad 
          Pucón 2023 - Plebiscito Constitucional")
grid.arrange(g21, g23)

p2_1 <- p %>% group_by(Comuna, RangoEtario) %>% summarise(value = trunc(sum(Votantes),2))
p3_1 <- dcast(p2_1, Comuna ~ RangoEtario)
p3_1 <- p3_1 %>% rename("18-19-2021" = "18-19", "20-29-2021" = "20-29", "30-39-2021" = "30-39",
                        "40-49-2021" = "40-49", "50-59-2021" ="50-59", "60-69-2021" = "60-69",
                        "70-79-2021" = "70-79", "80+-2021" = "80+")
pc2_1 <- pc %>% group_by(Comuna, Retario) %>% summarise(V2023 = trunc(sum(Votantes),2))
pc3_1 <- dcast(pc2_1, Comuna ~ Retario)
pc3_1 <- pc3_1 %>% rename("18-19-2023" = "18-19", "20-29-2023" = "20-29", "30-39-2023" = "30-39",
                        "40-49-2023" = "40-49", "50-59-2023" ="50-59", "60-69-2023" = "60-69",
                        "70-79-2023" = "70-79", "80+-2023" = "80+")


CompRang <- inner_join(p3_1, pc3_1, by="Comuna")
CompRang <- melt(CompRang, id.vars = "Comuna") 

g <- ggplot(CompRang, aes(fill=variable, y=value, x=Comuna)) + 
              geom_bar(stat = "identity", position = 'dodge')
g + geom_text(aes(label = value), 
              position = position_dodge(0.9),
              vjust = 0) +
  ggtitle("Total Comparado de Votantes Pucón por Rango de Edad 2021 - 2023")

write.csv(pls, file = "~/R/electoral/data/votacion_sexo_local_Pucon_2021.csv")
write.csv(plre, file = "~/R/electoral/data/votacion_Retario_local_Pucon_2021.csv")
write.csv(pls, file = "~/R/electoral/data/votacion_sexo_local_Pucon_2023.csv")
write.csv(pls, file = "~/R/electoral/data/votacion_Retario_local_Pucon_2023.csv")
