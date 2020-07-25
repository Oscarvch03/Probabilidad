# INTEGRANTES: Sebastian Martinez / Oscar Gomez / Oscar Velasco

# TAREA 1: Punto 2 R

library (gtools)
library (combinat)

Distribucion_Bernoulli<- function(p){
  m <- runif(1)
  if (m <= p){
    return (TRUE)
  }
  else{
    return (FALSE)
  }
}

Distribucion_Binomial <- function(n,p){
  cont <- 0
  for (i in 1:n){
    if (Distribucion_Bernoulli(p) == TRUE){
      cont  = cont+1
    }
  }
  return (cont)
}

Distribucion_Geometrica <- function(p){
  bool <- FALSE
  cont <- 0
  while(Distribucion_Bernoulli(p) == bool){
    cont <- cont+1
    if (Distribucion_Bernoulli(p) == TRUE ){
      bool <- TRUE
    }
  }
  return (cont)
}
#Distribucion_Poisson <- function(p){
#  for(i in 0:n){
#    (exp(-lambda)*lambda^i)/factorial(i)
# }
#}



p1 = 0.5
Distribucion_Bernoulli(p1)
Distribucion_Binomial(10,p1)
Distribucion_Geometrica(p1)
#Distribucion_Poisson(p1) # No entendimos como
