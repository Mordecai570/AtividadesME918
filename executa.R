library(purrr)
library(devtools)

source("simulador.R")

devtools::source_url("https://ime.unicamp.br/~ra137784/ME918/2023s2/lab01_ig.R")

planos <- input_lab01()

resultados <- purrr::map(planos, vetor_amos_aletoria)
