# datos
cantidad=c(100,300,600,1000,3000)
precio=c(15,7,6.5,6,5)

# dibujamos los datos
plot(cantidad,precio)
# Nos damos cuenta de que sigue una curva del tipo precio = p0 * exp (k/cantidad)
# transformo en log(precio) = log(p0) + k/cantidad
# si hago x = 1/cantidad , y = log(precio) --> m = k y n=log(p0). calculo k y p0

# Transformo
x=1/cantidad
y=log(precio)

# objeto que representa la linea de mejor ajuste
# regresión lineal
plot(x,y)
l1=lm(y~x)
abline(l1) # añado el dibujo al plot.


# si la recta es y = mx + n
m = l1$coefficients[2] # la pendiente es el segundo coeficiente
n = l1$coefficients[1] # la ordenada en el origen el primer coeficiente
print(paste0("y=",m,"x + ",n))
text(2,28,paste0("y=",a,"x + ",b),pos=4) # pone el texto en la gráfica

# resumen de l1
summary(l1)

# Valor de r
R2=summary(l1)$r.squared
print(paste0("El valor de R2 es: ",R2))

# calculo parámetros
k = m
p0 = exp(n)
print(paste0("Valor de k:  ",k))
print(paste0("Valor de p0:  ",p0))

# función para comprobar las preguntas
P = function(C) {p0 * exp(k/C)}
print(paste0("Precio cuando entren 500Kg de gambas ",P(500)))
print(paste0("Precio cuando entren 50K de gambas ",P(50)))


