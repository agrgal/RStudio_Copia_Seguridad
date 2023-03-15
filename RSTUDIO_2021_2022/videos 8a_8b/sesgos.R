# 1) cargar los datos e inspección
trabajadores = read.csv("datos_ejercicio.csv",header=FALSE)

#2) Tipos de cada dato
str(trabajadores)
#) Categoría es un factor de tres elementos, Sexo de dos y el sueldo un entero

#3) Cambio de los nombres de las columnas
names(trabajadores)=c("Categoría","Sexo","Sueldo")

#4) Número de empleados
dim(trabajadores)[1]

#5) Sueldo más bajo 
minimo = trabajadores[trabajadores$Sueldo==min(trabajadores$Sueldo),]
minimo

#6) Y más alto
maximo = trabajadores[trabajadores$Sueldo==max(trabajadores$Sueldo),]
maximo

#7) menos de mil euros
mileuristas = trabajadores[trabajadores$Sueldo<1000,]
dim(mileuristas)[1]

#8) Cuantos de ellos hombres / nujeres
dim(mileuristas[mileuristas$Sexo=="F",])[1]
dim(mileuristas[mileuristas$Sexo=="M",])[1]

#9) ¿Cuántos de ellos por cada categoría?
dim(mileuristas[mileuristas$Categoría=="I",])[1]
dim(mileuristas[mileuristas$Categoría=="II",])[1]
dim(mileuristas[mileuristas$Categoría=="III",])[1]

#10) Función table
t1 = table(mileuristas$Categoría,mileuristas$Sexo)

#11) Función table %
prop.table(t1)*100

#12) 50 mejores sueldos
em50 = trabajadores[order(trabajadores$Sueldo,decreasing = TRUE),]
em50 = em50[1:50,]

#promedio sueldos mujeres y hombres
mean(em50[em50$Sexo=="M",]$Sueldo)
mean(em50[em50$Sexo=="F",]$Sueldo)

# Tabla de frecuencias
prop.table(table(em50$Categoría,em50$Sexo))
