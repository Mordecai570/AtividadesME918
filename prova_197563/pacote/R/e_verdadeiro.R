#'
#'
#' @export


e_verdadeiro <- function(arg){
  if(arg[[1]] == 1){
   return(list(verdadeiro = TRUE))
  }
  else if(arg == 3){
   return(list(verdadeiro = TRUE))
  }
  else if(arg == 2){
   return(list(verdadeiro = FALSE, justificativa = "Cada arquivo em R pode conter mais funções"))
  }
  else if(arg == 4){
   return(list(verdadeiro = FALSE, justificativa = "roxygen2 só gera um arquivo de documentação"))
  }
}

