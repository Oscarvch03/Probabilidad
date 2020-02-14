clc

library (gtools)
library (combinat)

#2.

Distribucion_Binomial <- function(n,k,p){
  a = (p^k)*((1-p)^(n-k))
  b = nCm(n,k)
  c = b*a
  return (c)
}

Distribucion_Geometrica <- function(k,p){
  a = (1-p)^(k-1)
  b = p
  c = a*b
  return (c)
}

Distribucion_Poisson <- function(x,k){
  # x es lambda
  a = exp(-x)*(x^k)
  b = factorial(k)
  c = a / b
  return (c)
}

P <- runif(1,0,1)
N <- 4
K <- 2
X <- 2

Distribucion_Binomial(N,K,P)
Distribucion_Geometrica(K,P)
Distribucion_Poisson(X,K)

