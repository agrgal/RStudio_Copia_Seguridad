# Cargar los datos de pacientes
pacientes = read.csv("pacientes.csv",header=TRUE,dec=",")
edad = pacientes$Edad

# valores de tendencia central
# Cálculo de la moda
moda = which(table(edad)==max(table(edad)))
moda = as.numeric(names(moda))
moda 
max(table(edad))

# Cálculo de la media
mean(edad)

# Cálculo de la mediana
median(edad)

# medidas de posición 
# Cuantiles 
edad = sort(edad)
cumsum(prop.table(table(edad)))
quantile(edad,0.4)
# mediana o cuantil 0.5
quantile(edad, 0.5) 
median(edad) # O segundo cuartil 
# cuartiles --> 0.25, 0.5, 0.75 
quantile(edad, 0.25) 
quantile(edad, 0.75) 

# deciles y percentiles (cada décima)
quantile(edad, 0.1)
quantile(edad,0.01)

# Medidas de dispersión
diferencia = diff(range(edad))
IQR(edad)
quantile(edad,0.75)-quantile(edad,0.25)

# Varianza muestral
var(edad)
n=length(edad)
sum((edad-mean(edad))**2)/(n-1)
# Varianza típica
sum((edad-mean(edad))**2)/n
var(edad)*(n-1)/n

# Desviación típica muestral
sd(edad)
sqrt(var(edad))
sqrt(sum((edad-mean(edad))**2)/(n-1))
# Desviación típica normal
sd(edad)*sqrt((n-1)/n)
sqrt(sum((edad-mean(edad))**2)/(n))

#Summary
summary(edad)

boxplot(edad,names="caso\nA")
abline(h=mean(edad))