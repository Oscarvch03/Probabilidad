library (gtools)
library (combinat)

Distribucion_Binomial <- function(n,k,p){
  a = (p^k)*((1-p)^(n-k))
  b = nCm(n,k)
  c = b*a
  return (c)
}

# A) X tiene una Distribución Binomial, observe que tengo dos opciones para cada estudiante,
#    ha usado Wikipedia para al menos uno de sus trabajos academicos, o no la ha usado.

# B) 
x = 1:41
y <- rep(0,41)
for(k in 1:41){
    y[k] = Distribucion_binomial(41, k, 0.447)
}
  
plot(x, y)
