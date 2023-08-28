library(purrr)
library(devtools)
library(usethis)

## Passo 1

arg = list(distribution = "poisson", lambda = 2.0, obs = 20)
arg = list(distribution = "normal", mu = 1.2, sigma2 = 1.0, obs = 25)
arg = list(distribution = "bernoulli", p = 0.3, obs = 30)

f <- function(arg){
  if(arg[[1]] == "poisson"){
    a <- rpois(arg[[3]],arg[[2]]) 
    return(a)
  }else if(arg[[1]] == "bernoulli"){
    a <- rbinom(arg[[3]],1,arg[[2]])
    return(a)
  } else {
    sd <- sqrt(arg[[3]])
    a <- rnorm(arg[[4]],arg[[2]],sd) 
    return(a)
  }
}

vetor_amos_aletoria <- f

argumentos <- arg
