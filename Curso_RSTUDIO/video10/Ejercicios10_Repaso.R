#1) Cargar dataframe
datos = read.csv("futbolNEW.csv",header=TRUE)
str(datos)
#2) Añadir tres partidos a la lista
añadidos = data.frame(Casa=c(10,11,10),Visitante=c(0,8,9))
datos=rbind(datos,añadidos)

#3) Cálculo del nuevo vector QUINIELAS
datos$Quiniela = (datos$Casa>datos$Visitante)*1+(datos$Casa<datos$Visitante)*2
datos$Quiniela = as.factor(datos$Quiniela)
levels(datos$Quiniela) = c("X","1","2")
# datos$Quiniela=ordered(datos$Quiniela,levels=c("1","X","2"))

#4) tabla frecuencias absolutas ftable, con Quiniela en columnas
t1 = ftable(datos,col.vars = c("Quiniela"))
t1

#5) Tabla de frecuencias absolutas de partidos empatados
cat("\014")
empatados = datos[datos$Quiniela=="X",]
t2 = apply(table(empatados), MARGIN = 1,FUN=sum) 
#reduzco y quito la información de quinielas, y como la de Casa y 
# visitante es la misma en este caso, me quedo solo con la variación CASA
t2

#6) partidos empatados en total
sum(t2)

#7) partidos que ganaron los visitantes 0 - ...
visitantes = datos[datos$Quiniela=="2",]
t3 = apply(table(visitantes),MARGIN=c(1,2),FUN=sum)
t4 =t3["0",]

cat("\014")
#8) gráfica
colores = topo.colors(length(t4))
leyenda = paste0("0-",names(t4))
old.par=par()
par=(mar=c(0,0,0,0))
b1 = barplot (t4,main="Nº partidos 0-X ",beside=TRUE,xlab="X",ylab="Nº partidos",
              horiz=FALSE,col=colores, ylim=c(0,max(t4)+20),
              legend.text =leyenda,
              args.legend = list(x=8,y=105,cex=0.7,horiz=TRUE,text.col="darkgreen"))
text(b1,t4+2,t4,cex=0.9,pos=3)
par = old.par