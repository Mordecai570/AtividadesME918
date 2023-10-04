library(yaml)
library(prova197563)
library(jsonlite)

# Leitura arquivo json banco Penguins
jsonlite::read_json("~/Utilizando pacote da prova/data/penguins.json", simplifyDataFrame = TRUE) -> df
tibble::tibble(df)


# aplicando a função 2 a uma variável do banco

var_quad(df, "body_mass_g", integer)


# salvando o data frame em results

df <- var_quad(df, "body_mass_g", integer)

fname <- sprintf("~/Utilizando pacote da prova/df.RData")

save(df, file = fname)
