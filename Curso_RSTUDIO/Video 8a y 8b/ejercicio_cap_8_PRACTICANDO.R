# Ejercicio con vectores y dataframe (Cap 8)
# ==========================================

#1)
empleados = read.csv("datos_ejercicio.csv",header=FALSE)
head(empleados,20)

#2) Cambiar nombre campos
names(empleados)=c("Categoría","Sexo","Sueldo")

#4) Descripción del dataframe
cat("\014")
str(empleados)

#5) Sueldo más bajo
min(empleados$Sueldo)
empleados[which(empleados$Sueldo==min(empleados$Sueldo)),]

#6) Sueldo más bajo
max(empleados$Sueldo)
empleados[which(empleados$Sueldo==max(empleados$Sueldo)),]

#7) Mileuristas
cat("\014")
dim(empleados[empleados$Sueldo<1000,])
#7.1) 
dim(empleados[empleados$Sueldo<1000 & empleados$Sexo=="F",])
dim(empleados[empleados$Sueldo<1000 & empleados$Sexo=="M",])
#7.2) 
dim(empleados[empleados$Sueldo<1000 & empleados$Categoría=="I",])
dim(empleados[empleados$Sueldo<1000 & empleados$Categoría=="II",])
dim(empleados[empleados$Sueldo<1000 & empleados$Categoría=="III",])

#8) Mileuristas
e2=empleados[empleados$Sueldo<1000,]
tHM = table(e2$Sexo)
tCAT = table(e2$Categoría)
t2 = table(e2$Sexo,e2$Categoría)
tHM
tCAT
t2

#9) Porcentajes
prop.table(tHM)*100
prop.table(tCAT)*100
prop.table(t2)*100

#10) 50 mejores sueldos
eTOP = empleados[order(empleados$Sueldo,decreasing = TRUE),][1:50,]
tTOP=table(eTOP$Sexo,eTOP$Categoría)
prop.table(tTOP)*100

#11) Diferencia entre el sueldo promedio masculino y femenino
mean(eTOP[eTOP$Sexo=="M",]$Sueldo)-mean(eTOP[eTOP$Sexo=="F",]$Sueldo)