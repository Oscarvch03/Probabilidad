clc <- function(){
  cat("\014")
}

clc()


# Distribucion de Poisson 
Poisson <- function(k, lambda){
    Pk = exp(-lambda) * (lambda ^ k) / factorial(k)
    return(Pk)
}


####################################################################


# Tenemos:
# 210 automoviles en promedio por hora
# Es decir 3.5 automoviles en promedio por min

# Sea X v.a que me determina el numero de automoviles
# que llegan a la estacion de gasolina en un minuto dado

# Observe que X tiene Distribucion de Poisson con
# parametro lambda = 3.5

# Calcular P(X > 10) en un minuto dado
# Observe que P(X > 10) = 1 - P(X <= 10)
# Asi:

p = 0
lambda = 3.5
for(k in 0:10){
    p = p + Poisson(k, lambda)
}

P = 1 - p

cat('Por lo tanto, P(X > 10) = 1 - P(X <= 10) = ', P)
