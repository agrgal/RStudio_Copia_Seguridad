z=seq(0,10,0.5)

# Funciones logarítmicas
f1=function(x){log10(x)}
f2=function(x){log(x)}
plot(z,f1(z),col="red",pch=5,ylim=c(0,f2(10)))
curve(f2(z),xname="z",type="p",col="green",pch=5,add=TRUE)

# Funciones exponenciales
f1=function(x){exp(x)}
f2=function(x){3^x}
plot(z,f1(z),col="red",pch=5,ylim=c(0,f2(10)))
curve(f2(z),xname="z",type="p",col="green",pch=5,add=TRUE)

# Funciones exponenciales exponente menor que 1
f1=function(x){sqrt(x)}
f2=function(x){x^0.5}
plot(z,f1(z),col="red",pch=5)
curve(f2(z),xname="z",type="p",col="green",pch=5,add=TRUE)


# Funciones polinómicas
f1=function(x){(x^2)+20}
f2=function(x){x^3}
plot(z,f1(z),col="red",pch=5,ylim=c(0,f2(10)))
curve(f2(z),xname="z",type="p",col="green",pch=5,add=TRUE)
