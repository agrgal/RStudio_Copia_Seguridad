# datos
p=c(5, 	6,	7,	8,	9,	10,	11,	12,	13,	14,	15,	16,	17,	18,	19)
V=c(13.51,	11.00,	8.70,	8.44,	7.50,	6.30,	6.14,	5.63,	5.19,	4.82,	4.50,	4.22,	4.00,	3.60,	3.55)
invV=1/V
T = 305
R = 0.082

# dibujamos los datos
plot(invV,p) # invV es la X y p es la Y

# objeto que representa la linea de mejor ajuste
l1=lm(p~invV)
abline(l1) # añado el dibujo al plot.


# si la recta es y = ax + b
a = l1$coefficients[2] # la pendiente es el segundo coeficiente
b = l1$coefficients[1] # la ordenada en el origen el primer coeficiente
print(paste0("y=",a,"x + ",b))
text(invV[1],p[9],paste0("y=",a,"x + ",b),pos=4) # pone el texto en la gráfica

# resumen de l1
summary(l1)

# Valor de r
R2=summary(l1)$r.squared
print(paste0("El valor de R2 es: ",R2))

# cálculo del número de moles
# p = nRT / V  y se corresponde a = nRT, luego  n = a/ RT

n = a / (R*T)
print(paste0("El número de moles es: ",n))

