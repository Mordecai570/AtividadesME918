library(yaml)

#Caso 1
library(prova197563)

#Caso 2
#Dado que o nosso arquivo tar.gz já está na pasta do projeto
devtools::install_local("prova197563_0.1.0.tar.gz")
library(prova197563)


# Pré-requisitos Probabilidade II
leitura$cursos$`estatística`$disciplinas[[2]]$`pre-requisitos`

library(jsonlite)

# Leitura arquivo json banco Penguins
jsonlite::read_json("penguins.json", simplifyDataFrame = TRUE) -> df_penguins
tibble::tibble(df_penguins)

# Leitura arquivo json banco Diamonds
jsonlite::read_json("diamonds.json", simplifyDataFrame = TRUE) -> df_diamonds
tibble::tibble(df_diamonds)
