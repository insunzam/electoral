library(dplyr)
library(tidyverse)
library(dslabs)
library(gridExtra)
library(sqldf)
library(reshape2)

#Carga de datos
load("data/terr_elec.RData")
load("data/V2023_PConst_VXL.RData")

te <- Territorios_Electorales
te$Comuna <- toupper(te$Comuna)
te <- te %>% rename("CIRCSEN" = "Circunscripción Senatorial", "CIRCPROV" = "Circunscripción Provincial", "DISTRITO" = "Distrito")  
te$Comuna <- chartr("ÁÉÍÓÚ", "AEIOU", toupper(te$Comuna))

dp <- votacion_2023_pc_xlocal
dp$BLYNU <- (dp$VOTOS.EN.BLANCO+dp$VOTOS.NULOS)
dp <- inner_join(dp,te, by="Comuna")
write.csv(dp, file = "~/R/electoral/data/votacionxlocal_2023.csv")

#Por comuna
dp <- dp %>% filter(Comuna %in% c("VILCUN") )
#Por Circunscripción Provincial
dp <- dp %>% filter(CIRCPROV == c("Malleco") )

dp <- dp %>% select(Local, A.FAVOR, EN.CONTRA, BLYNU)
dp0 <- melt(dp, id.vars = "Local")

g <- ggplot(dp0, aes(fill=variable, y=value, x=Local)) +
  geom_bar(stat = "identity", position = 'dodge') 
g + geom_text(aes(label = value), position = position_dodge(0.9),
              vjust = 2, color = "white", size=4) +
  theme(axis.text.x = element_text(angle = 90, size = 6)) +
  ggtitle("Votacion por Local Vilcun Plebiscito - 2023")



dp1 <- dcast(dp, Comuna ~ Local, value.var = "A.FAVOR")
dp2 <- dcast(dp, Comuna ~ Local, value.var = "EN.CONTRA")
dp3 <- dcast(dp, Comuna ~ Local, value.var = "BLYNU")








p <- X2021_11_CR_DE_DV %>% 
  filter(NumRegion == 9 & Comuna %in% c("VILCUN")) %>% 
  select(Comuna, Sexo, Mesa, RangoEtario, Nacionalidad, Votantes)
pc <- X2023_PConst_DV %>% 
  filter(NumRegion == 9 & Comuna %in% c("VILCUN")) %>% 
  select(Comuna, Sexo, Mesa, Retario, Nacionalidad, Votantes)

p0 <- p %>% group_by(Comuna) %>% summarise(value = sum(Votantes))
ToT <- p0$value
p$P_Votantes <- p$Votantes / ToT
p1 <- p %>% group_by(Comuna, Sexo) %>% summarise(value = sum(P_Votantes))

pc0 <- p %>% group_by(Comuna) %>% summarise(value = sum(Votantes))
ToT_1 <- pc0$value
pc$P_Votantes <- pc$Votantes / ToT_1



g <- ggplot(dp, aes(fill=Local, y=value, x=Comuna)) +
  geom_bar(stat = "identity", position = 'dodge') 
g + geom_text(aes(label = value), position = position_dodge(0.9),
              vjust = 0, color = "blue", size=4) +
  ggtitle("Total Comparado de Votantes Vilcún por Sexo - 2021 - 2023")

