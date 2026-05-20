library(dplyr)
library(tidyverse)
library(dslabs)

escuelas <- read.csv("~/R/electoral/fuentes/escuelas_araucania.csv", sep = ";", fileEncoding = "UTF-8-BOM")
resgob24 <- read.csv("~/R/electoral/fuentes/araucania_gobernadores2024.csv", fileEncoding = "UTF-8-BOM")
