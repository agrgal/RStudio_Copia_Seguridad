# cargando los valores del fichero resultado
calificaciones = read.csv("resultado.csv",header=FALSE,dec=",")
head(calificaciones)

# cambio de nombre
names(calificaciones)=c("Notas","Sexo")

# ¿cuántos opositores?
dim(calificaciones)[1]
str(calificaciones)

# Nota más baja
min(calificaciones$Notas)
# Nota más alta
max(calificaciones$Notas)

# Filas en las que están los ceros
which(calificaciones$Notas==0)
calificaciones$Sexo[which(calificaciones$Notas==0)] # sexos de esa personas

# Notas máximas
which(calificaciones$Notas==9.98)
calificaciones$Sexo[which(calificaciones$Notas==9.98)] # sexos de esa personas

calificaciones$Sexo[which(calificaciones$Notas==max(calificaciones$Notas))] # sexos de esa personas

# ¿Cuántos han aprobado?
length(which(calificaciones$Notas>=5))
length(which(calificaciones$Notas>=5 & calificaciones$Sexo=="F"))
length(which(calificaciones$Notas>=5 & calificaciones$Sexo=="M"))

#Tabla de calificaciones absolutas
t1 = table(calificaciones$Sexo)
t1["F"]

prop.table(t1)

# Calificaciones de los 23 primeros
calOrdenadas=calificaciones[order(calificaciones$Notas,decreasing = TRUE),]
mejores=calOrdenadas[1:23,]
min(mejores$Notas)

# Mujeres que han aprobado
t2=table(mejores$Sexo)
t2
prop.table(t2)

# Gráficas
barplot(table(calificaciones$Sexo),col=c("green","blue"),
        legend.text = c("Mujer","Hombre"),
        args.legend = list(x=0.5,y=600,cex=1),
        xlab="Sexos",ylab="Números")
pie(table(calificaciones$Sexo),col=c("green","red"))
