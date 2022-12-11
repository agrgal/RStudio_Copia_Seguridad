# datos
sal=c(1,2,3,4,5,6,7,8,8.5,9)
tiempo=c(22,23,24,25,26,27,27.5,27.8,28,29)

# dibujamos los datos
plot(sal,tiempo)

# objeto que representa la linea de mejor ajuste
l1=lm(tiempo~sal)
abline(l1) # añado el dibujo al plot.


# si la recta es y = ax + b
a = l1$coefficients[2] # la pendiente es el segundo coeficiente
b = l1$coefficients[1] # la ordenada en el origen el primer coeficiente
print(paste0("y=",a,"x + ",b))
text(2,28,paste0("y=",a,"x + ",b),pos=4) # pone el texto en la gráfica

# resumen de l1
summary(l1)

# Valor de r
R2=summary(l1)$r.squared
print(paste0("El valor de R2 es: ",R2))


