# Sabemos que:

#       X1 + X2 + ... + Xn-1 + Xn
# Sn = ---------------------------
#                 n                  


# a) Dem //


# Sea: 
#         X1 + X2 + ... + Xn-1
# Sn-1 = ----------------------
#               n - 1

# Luego:
#       X1 + X2 + ... + Xn-1     Xn
# Sn = ---------------------- + -----
#                 n               n
#
# Multiplicamos la primera parte por 1 =  n - 1
#                                        -------
#                                         n - 1

# Así:
#      (n - 1) (X1 + X2 + ... + Xn-1)    Xn
# Sn = ------- ---------------------- + -----
#      (n - 1)            n               n

#      (n - 1) (X1 + X2 + ... + Xn-1)    Xn
# Sn = ------- ---------------------- + -----
#         n           (n - 1)             n

# Por lo tanto:

#       n - 1             1   
# Sn = ------- (Sn-1) + ----- (Xn)   
#         n               n          Sol //



# b)

clc <- function(){
  cat("\014")
}

clc()


Sn <- function(n){
    S = 0
    for(i in 1:n){
        a = runif(1, 0, 1)
        b = runif(1, 0, 1)
        # Estamos considerando el cuadrante 1 del plano 
        # cartesiano, es decir 1/4 del disco unitario
        if(a^2 + b^2 <= 1){
            Xi = 1 
        }
        else{
            Xi = 0
        }
        
        # Xi es el valor que toma la i-esima v.a
        
        S = ((i - 1) / i * S) + (Xi / i)  
    }
    return(S)
}


# c) Calculemos una aproximacion de pi:


# Sabemos que el area de un circulo A(C) = pi * r^2

# En este caso r = 1, porque tenemos el disco unitario

# Ademas solo estamos usando 1/4 de dicho circulo, por
# tanto A(C/4) = pi / 4

# Luego tenemos que por aproximacion, Sn(n) ~~= pi / 4
# Despejando 4 * Sn(n) ~~= pi

# Tomemos n = 10mill

Sn10Mill = Sn(10000000)  # Se demora corriendo 30seg aprox

pi = 4 * Sn10Mill

cat('Tomando n = 10Mill, la aproximación de Pi es: ', pi)

# No es una buena aproximacion :(
