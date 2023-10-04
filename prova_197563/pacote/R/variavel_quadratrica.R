#' @title variável_quadratrica
#' @import dplyr
#' @param df Um dataframe .
#' @param variavel Uma string indicando qual e a coluna de df a ser plotada
#' @param tipo O tipo do grafico a ser plotado
#'
#' @return Retorna um data frame com uma variável ao quadrado
#' @examples var_quad(penguinson,"body_mass_g",integer)
#' @export

#Critérios de parada

var_quad<-function(df,variavel,tipo){

  if ( !(variavel %in% (colnames(df)) )) {
    stop(paste0("coluna ", variavel, " nao existem em df"))

  }
  else if( class(df[,variavel]) != "numeric" & class(df[,variavel]) != "integer"){
    stop(paste0(variavel ," não é do tipo numeric"))
  }
  else if( class(df[,variavel]) != "integer"){
    stop(paste0(variavel ," não é do tipo integer"))
  }
  df$quad <- df[,variavel]^2
  return(df)
}

