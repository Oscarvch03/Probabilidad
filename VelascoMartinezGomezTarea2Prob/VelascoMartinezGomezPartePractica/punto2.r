library (gtools)
library (combinat)

clc <- function(){
  cat("\014")
}

clc()

# Distribucion Binomial
Binomial <- function(n, k, p){
    Pk = nCm(n, k) * (p ^ k) * ((1 - p) ^(n - k))
    return(Pk)
}

# Distribucion de Poisson 
Poisson <- function(k, lambda){
    Pk = exp(-lambda) * (lambda ^ k) / factorial(k)
    return(Pk)
}


####################################################################


# Tenemos:
# La poblacion de individuos con renta superior a 2 mill de dolares 
# es de 0.005%.

# Determine la probabilidad de que entre 5000 individuos haya como 
# mucho 2 con ese nivel (Es decir 0, 1, 2 individuos) 

# Sea X v.a que me determina el numero de individuos que cumplen 
# la condicion anterior.
# Determinar P(X <= 2)


n = 5000
k = 2
p = 0.005
lambda = n * p
cat('lambda = ', lambda)

# Binomial
PB = 0
for(i in 0:k){
    PB = PB + Binomial(n, i, p)
}

cat('Usando Binomial: P(X <= 2) = ', PB)


# Poisson, lambda = np
PP = 0
for(j in 0:k){
  PP = PP + Poisson(j, lambda)
}

cat('Usando Poisson: P(X <= 2) = ', PP)


