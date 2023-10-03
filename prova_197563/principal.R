library(yaml)
library(prova197563)
library(jsonlite)

# Leitura arquivo json banco Penguins
jsonlite::read_json("penguins.json", simplifyDataFrame = TRUE) -> df_penguins
tibble::tibble(df_penguins)

# Leitura arquivo json banco Diamonds
jsonlite::read_json("diamonds.json", simplifyDataFrame = TRUE) -> df_diamonds
tibble::tibble(df_diamonds)
