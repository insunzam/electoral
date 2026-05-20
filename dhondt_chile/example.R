source("~/R/electoral/dhondt_chile/dhondt_Chile.R")
library("electoral")

data <- read.csv("~/R/electoral/dhondt_chile/vote_candidate_2025.csv", fileEncoding = "UTF-8-BOM")

traza <- dhondt_chile(data, 9, 5)
trace("traza", tracer = substitute(print(as.list(match.call()))),
      exit = substitute(print(returnValue())))
