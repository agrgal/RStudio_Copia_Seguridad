#1) Cargar dataframe
datos = read.csv("futbolNEW.csv",header=TRUE)
str(datos)
#2) Añadir tres partidos a la lista
añadidos = data.frame(Casa=c(10,11,10),Visitante=c(0,8,9))
datos=rbind(datos,añadidos)

  # #2a) tabla de frecuencias absolutas y relativas (EJERCICIO 9)
  # t1 = table(datos)
  # t2 = 100*prop.table(t1)
  # 
  # #2b) Resultados que no se han producido
  # resultados = data.frame(t1)
  # resultados=resultados[resultados$Freq==0,]
  # resultados
  # 
  # #2c) Resultado de más frecuencia
  # resultados = data.frame(t1)
  # resultados[resultados$Freq==max(resultados$Freq),]
  # 
  # #2d) ¿Con qué frecuencia relativa (porcentaje) se da los partidos en los que el equipo de casa no marca un gol?
  # t3 = apply(t2,MARGIN=1,FUN=sum)
  # t3=round(t3,2)
  # 
  # #2e) ¿Con qué frecuencia relativa (porcentaje) se da los partidos en los que el equipo visitante no marca un gol? Usa la función sum o cumsum, el método apply y MARGIN adecuado. 
  # t4 = apply(t2,MARGIN=2,FUN=sum)
  # t4=round(t4,2)
  # 
  # #2f) Gráfica de los partidos 0-X
  # # Gráfica de los partidos cuyo resultado es  0 - X, según los goles de los visitantes
  # partidos0X = t1["0",]
  # colores=topo.colors(length(partidos0X))
  # old.par=par()
  # par=(mar=c(0,0,0,0))
  # migrafica = barplot(partidos0X,main="Nº partidos 0-X ",beside=TRUE,xlab="X",ylab="Nº partidos",
  #                     horiz=FALSE,col=colores, ylim=c(0,max(partidos0X)+20),
  #                     legend.text =names(t1["0",]),
  #                     args.legend = list(x=20,y=40,cex=0.5,horiz=TRUE,text.col="darkgreen"))
  # text(migrafica, partidos0X + 0.1 , partidos0X, cex=0.6,pos=3) 
  # par = old.par
  
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
t2 = table(empatados)
t3 = apply(t2, MARGIN = 1,FUN=sum) 
#reduzco y quito la información de quinielas, y como la de Casa y 
# visitante es la misma en este caso, me quedo solo con la variación CASA
t3

#6) partidos empatados en total
sum(t3)
t2 = t3

#7) partidos que ganaron los visitantes 0 - ...
visitantes = datos[datos$Quiniela=="2",]
t3 = apply(table(visitantes),MARGIN=c(1,2),FUN=sum)
t4 =t3["0",]

cat("\014")

#8) gráfica
colores = terrain.colors(length(t2))
leyenda = paste0("0-",names(t2))
old.par=par()
par=(mar=c(0,0,0,0))
b1 = barplot (t2,main="Nº partidos X-X ",beside=TRUE,xlab="X",ylab="Nº partidos",
              horiz=FALSE,col=colores, ylim=c(0,max(t4)+20),
              legend.text =leyenda,
              args.legend = list(x=8,y=105,cex=0.7,horiz=TRUE,text.col="darkgreen"))
text(b1,t2+2,t2,cex=0.9,pos=3)
par = old.par

#9) gráfica
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