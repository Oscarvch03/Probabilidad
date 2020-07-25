# INTEGRANTES: Sebastian Martinez / Oscar Gomez / Oscar Velasco

# TAREA 1: Punto Q R


library (gtools)
library (combinat)

Distribucion_Binomial <- function(n,k,p){
  a = (p^k)*((1-p)^(n-k))
  b = nCm(n,k)
  c = b*a
  return (c)
}


n = 41
p = 0.447


# A) X tiene una Distribución Binomial, observe que tengo dos opciones para cada estudiante,
#    ha usado Wikipedia para al menos uno de sus trabajos academicos, o no la ha usado.


# B)

x = 1:42
y <- rep(0,42)
for(k in 0:n){
  y[k+1] = Distribucion_Binomial(n, k, p)
}

plot(x, y,
     main = "Función de masa de Probabilidad Binomial \n con n = 41 & p = 0.447",
     xlab = "X = x",
     ylab = "PX(x)", type = "o", col = "blue")
# La Grafica de la funcion de masa de probabilidad se parece mucho a la Campana de Gauss.


# C)

x17 = Distribucion_Binomial(n, 17, p)
cat("La Probabilidad de que X = 17 es PX(x = 17) = ", x17)


# D)

xmei13 = 0
for(k in 0:13){
  xmei13 = xmei13 + Distribucion_Binomial(n, k, p)
}
cat("La Probabilidad de que X <= 13 es PX(x <= 13) = ", xmei13)


# E)

xma11 = 0;
for(k in 12:n){
  xma11 = xma11 + Distribucion_Binomial(n, k, p)
}
cat("La Probabilidad de que X > 11 es PX(x > 11) = ", xma11)


# F)

xin16and19 = 0
for(k in 16:19){
  xin16and19 = xin16and19 + Distribucion_Binomial(n, k, p)
}
cat("La Probabilidad de que 16 <= X <= 19 es PX(16 <= x <= 19) = ", xin16and19)
