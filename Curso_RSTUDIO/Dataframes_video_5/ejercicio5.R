# Ejercicio video 5
df_pearson=read.table("http://aprender.uib.es/Rdir/pearson.txt",header=TRUE)

# 5 filas al comienzo
head(df_pearson)

# análisis de regresión
l1=lm(Hijos~Padres,data=df_pearson)
plot(df_pearson)
abline(l1)
a = summary(l1)
a$r.squared

# comandos para ver
names(df_pearson) #vemos nombres de columnas
rownames(df_pearson) #identificadores de registros o filas
dimnames(df_pearson) #ambas a la vez: identificadores filas y columnas
dim(df_pearson) #dimensiones

#subconjuntos de datos: columnas
df_pearson$Padres #los datos de altura de los padres
df_pearson[,1] #de forma idéntica: primera columna
df_pearson[,c(1)] #de forma idéntica, por vector
df_pearson[,c(1,2)] #todo el dataframe

#subconjuntos de datos: filas
df_pearson[c(12,15),] # filas 12 y 15
df_pearson[12:15,]#filas de la 12 a la 15

#filas con condiciones
cat("\014")
df_pearson[df_pearson$Padres>=65.5 & df_pearson$Padres<65.7,]
df_pearson[df_pearson$Padres>=65.5 & df_pearson$Padres<65.7,][3:5,]
