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
t2=table(e2$Sexo,e2$Categoría) #todavía puede que no lo hagan así
tSexo=table(e2$Sexo)
tCat=table(e2$Categoría)
t2
tSexo
tCat

#9) Mileuristas. Porcentajes
prop.table(t2)
prop.table(tSexo)
prop.table(tCat)

#10.1) Empleados top
cat("\014")
emTop = empleados[order(empleados$Sueldo,decreasing = TRUE),]
em50 = emTop[1:50,]
prop.table(table(em50$Categoría,em50$Sexo))
#10.2)
mean(em50$Sueldo[em50$Sexo=="M"])-mean(em50$Sueldo[em50$Sexo=="F"])
#10,3)
graf1 <-barplot(table(em50$Sexo),
        col=c("blue","orange"),
        legend.text=c("Mujeres","Hombres"),
        args.legend = list(x=0.8,y=40,cex=1),
        xlab="Sexos",ylab="Número",
        ylim=c(0,35))
text(x=graf1,y=table(em50$Sexo)+1,labels=table(em50$Sexo))

pie(prop.table(table(em50$Sexo)),
    col=c("blue","orange"),
    labels=paste(as.character(100*prop.table(table(em50$Sexo))),"%"),
    )
legend("left",c("Mujer","Hombre"),fill=c("blue","orange"))



