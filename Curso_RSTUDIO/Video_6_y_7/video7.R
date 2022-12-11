# Ejercicio sobre dataframes
cat("\014")
rm(list = ls()) # borrar el environment

# Ejemplo
a = c(3,4,5)
b=c(7,1,2)
concatenado = c(a,b)
concatenado = c(a,b,b,c(2,8,-3))

acuadrado = a^2
sin(a)
f1=function(x){x^2 + 25*x +8}
f1(a)

# más
a + b
a * b
sum(a)
mean(a)
median(a)
cumsum(a)

# ordenando
a=c(3,8,1,0,21)
sort(a) # ordenar 
sort(a,dec=TRUE) # ordenar descendentemente
rev(a) # reverso

# dfasd
a
a[3] # elemento posición 3
a[-3] # todos los elementos menos el de la posición 3
a[-c(2,5)] # menos los elementos 2 y 5
a[2:4] # los elementos 2, 3, 4
a[a>5] # los elementos cuyo valor sea mayor que 5
a[a>1 & a<8] # valores mayores que uno y menores que 8
a[a<2 | a>8] # valores menores que dos a mayores que 8
a[!(a>5)] # valores menosres o igual que 5


