# Ejercicio sobre dataframes
cat("\014")

# Ejemplo
edades=c(12,15,17,13,18,20)
ejemplo1=c("Hola","Adiós","Hasta luego")
ejemplo2=c(TRUE,TRUE,FALSE,FALSE,TRUE,TRUE)
notas = scan("http://aprender.uib.es/Rdir/notas.txt")
notas = scan("http://aprender.uib.es/Rdir/notas.txt",what="character",encoding="UTF-8")

# repetición
a = rep(c(1,2,3),3)
b= rep(c(1,2,3),each=4)
c = rep(c(1,2,3),times=c(4,7,9))

# Con seq
a = seq(3.2,22.3,by=4)
b = seq(22,8,by=-1.2)
c = seq(1,8,length.out=12)
d = seq(2,by=3,length.out=10)
e = 1:4
f = 5:-2
g = -(1:8)