# Ejercicio sobre dataframes
cat("\014")
calidadAire = airquality

# Obtención del subdataframe desechando las dos primeras columnas
calidadAire2 = calidadAire[,-(1:2)]

# A) dimensiones
dim(calidadAire2)
calidadAire2$Fecha=paste0(calidadAire2$Day,"-",calidadAire2$Month)

#B) regresión lineal
l1=lm(Temp~Wind,data=calidadAire2)
plot(calidadAire2$Wind,calidadAire2$Temp,xlim=c(0,30),ylim=c(0,100))
abline(l1)

#C) pendiente de la recta
m = l1$coefficients[2]
n = l1$coefficients[1]
rcuadrado = summary(l1)$r.squared
print(paste0("la pendiente de la recta es: ",m))
print(paste0("la ordenada de la recta es: ",n))
print(paste0("el coeficiente R2 de la recta es: ",rcuadrado))

#D) días del período en el que la temperatura es mayor de 90ºF
temp90omas = calidadAire2[calidadAire2$Temp>=90,]$Fecha
length(temp90omas)
temp90omas
