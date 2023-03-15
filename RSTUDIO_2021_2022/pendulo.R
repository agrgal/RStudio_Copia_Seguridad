# 1) Datos
l=c(100,	110,	120,	130,	140,	150,	160,	170,	180,	190,	200,	210,	220,	230,	240,	250,	260)
T=c(2.01,	2.11,	2.20,	2.29,	2.37,	2.46,	2.5,	2.62,	2.69,	2.79,	2.84,	2.91,	3.2,	3.04,	3.11,	3.17,	3.14)
l = l /100 # porque está en centímetros y hay que pasar a metros

# 2) representación
plot(l,T) # l variable independiente, T dependiente.
#parece una gráfica tipo y = A x ^ {1/b}

# 3) Transformación en recta
l1=lm(log(T)~log(l))
plot(log(l),log(T))
abline(l1)
m = l1$coefficients[2]
n = l1$coefficients[1]
# 3a) compruebo m
print(m) #) Sí, es casi 0.5 --> VAMOS BIEN

# 4) Cálculo de g
# n = ln (2 pi / raiz(g) )
g = (4 * pi **2) / exp (2*n)
print(paste0("La gravedad es: ",g))

#5) Dibujo de la gráfica REAL
plot(l,T)
f1=function(l) {2*pi*(l/g)**0.5}
curve(f1(l),xname="l",add=TRUE)

#6) Fiabilidad
summary(l1)$r.squared