library(yaml)
library(prova197563)
library(jsonlite)

# Leitura arquivo json banco Penguins
jsonlite::read_json("~/Utilizando pacote da prova/data/penguins.json", simplifyDataFrame = TRUE) -> df
tibble::tibble(df)
