cantidad=c(100,300,600,1000,3000)
precio=c(15,7,6.5,6,5)
plot(cantidad,precio)

# transformar
y = log(precio)
x = 1 / cantidad

# recta de represión
plot(x,y)
l1 = lm(y~x)
abline(l1)

# coeficientes
m = l1$coefficients[2]
n = l1$coefficients[1]

# los datos
k = m
k
po = exp(n)
po

# funcion
f1=function(x){po* exp(k/x)}
f1(50)

# función
plot(cantidad,precio)
curve(f1(cantidad),xname="cantidad",add=TRUE)
summary(l1)$r.squared