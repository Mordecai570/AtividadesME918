library(purrr)
library(devtools)

source("executa.R")

salvar <- function(i){
  arquivo<-paste0(sprintf("%02d",i), ".RData")
  resultados <- resultados[[i]]
  distribution <- planos[[i]]$distribution
  obs <- planos[[i]]$obs
  caso_normal <- function(x){
    sigma2 <- x$sigma2
    mu <- x$mu
    save(sigma2,mu,obs,distribution,resultados, file= arquivo)
  }
  caso_poisson <- function(x){
    lambda <- x$lambda
    save(lambda,obs,distribution,resultados, file= arquivo)
  }
  caso_binomial <- function(x){
    p <- x$p
    save(p,obs,distribution,resultados, file= arquivo)
  }
  switch (planos[[i]]$distribution,
    "normal" = caso_normal(planos[[i]]),
    "poisson" = caso_poisson(planos[[i]]),
    "bernoulli" = caso_binomial(planos[[i]])
  )
}

walk(seq_along(planos),\(i) salvar(i))

