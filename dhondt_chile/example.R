source("~/R/electoral/dhondt_chile/dhondt_Chile.R")
library("electoral")

data <- read.csv("~/R/electoral/dhondt_chile/vote_candidate_2021.csv", fileEncoding = "UTF-8-BOM")

traza <- dhondt_chile(data, 23, 7)
trace("traza", tracer = substitute(print(as.list(match.call()))),
      exit = substitute(print(returnValue())))
