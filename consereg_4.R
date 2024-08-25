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

te <- Territorios_Electorales %>% rename("COMUNA" = "Comuna")
te$COMUNA <- toupper(te$COMUNA)
te <- te %>% rename("CIRCSEN" = "Circunscripción Senatorial", "CIRCPROV" = "Circunscripción Provincial", "DISTRITO" = "Distrito")  

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







x2 <- pi * 100^(-1:3)
round(x2, 3)
signif(x2, 3)
