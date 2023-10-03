library(yaml)
library(prova197563)
library(jsonlite)

# Leitura arquivo json banco Penguins
jsonlite::read_json("penguins.json", simplifyDataFrame = TRUE) -> df
tibble::tibble(df)
