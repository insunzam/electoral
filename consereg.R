library(dplyr)
library(tidyverse)
library(dslabs)
library(gridExtra)
library(sqldf)
library(reshape2)

#setwd("/home/insunzam/R/electoral")
load("data/CR_2021_DV.RData")
load("data/PCon_2023_DV.RData")
load("data/porc_sexo_2123.RData")
load("data/terr_elec.RData")

ps <- part_sex_21_23
ps$PH2021 <- (ps$PH2021*100)
ps$PM2021 <- (ps$PM2021*100)
ps$PH2023 <- (ps$PH2023*100)
ps$PM2023 <- (ps$PM2023*100)
te <- Territorios_Electorales %>% rename("COMUNA" = "Comuna")
te$COMUNA <- toupper(te$COMUNA)
te <- te %>% rename("CIRCSEN" = "Circunscripción Senatorial", "CIRCPROV" = "Circunscripción Provincial", "DISTRITO" = "Distrito")  
ps <- inner_join(ps,te, by="COMUNA")

#Malleco
psm <- ps %>% filter(CIRCPROV == "Malleco") %>% 
  select(COMUNA, PH2021, PM2021, PH2023, PM2023)

CompSex <- melt(psm, id.vars = "COMUNA") 

g <- ggplot(CompSex, aes(fill=variable, y=value, x=COMUNA)) +
  geom_bar(stat = "identity", position = 'dodge') 
g + geom_text(aes(label = value), 
              position = position_dodge(0.9),
              vjust = 0)
#Cautin I
psci <- ps %>% filter(CIRCPROV == "Cautin I") %>% 
  select(COMUNA, PH2021, PM2021, PH2023, PM2023)

CompSex <- melt(psci, id.vars = "COMUNA") 

g <- ggplot(CompSex, aes(fill=variable, y=value, x=COMUNA)) +
  geom_bar(stat = "identity", position = 'dodge') 
g + geom_text(aes(label = value), 
              position = position_dodge(0.9),
              vjust = 0)

#Cautin II
pscii <- ps %>% filter(CIRCPROV == "Cautin II") %>% 
  select(COMUNA, PH2021, PM2021, PH2023, PM2023)

CompSex <- melt(pscii, id.vars = "COMUNA") 

g <- ggplot(CompSex, aes(fill=variable, y=value, x=COMUNA)) +
  geom_bar(stat = "identity", position = 'dodge') 
g + geom_text(aes(label = value), 
              position = position_dodge(0.9),
              vjust = 0)
