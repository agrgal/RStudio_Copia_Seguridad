masa=c(0.025, 0.3, 3, 30, 65, 200, 3500, 1500, 15)
edad =c(3.5, 7.5, 14, 18, 70, 35, 70, 50, 7 )
plot(log(masa),log(edad))

# Recta de regresión
l1=lm(log(edad)~log(masa))
abline(l1)

# ecuación probable y = Ax^(1/b)
m = l1$coefficients[2]
n = l1$coefficients[1]
b = 1/m
print(paste0("El coeficiente b es: ",b))
A = exp(n)
print(paste0("El coeficiente A es: ",A))

summary(l1)$r.squared

# Dibujo de los puntos y la ecuación
plot(masa,edad)
f1 = function(x) {A*x^{1/b}}
curve(f1(masa),xname="masa",type="p",col="green",pch=5,add=TRUE)
