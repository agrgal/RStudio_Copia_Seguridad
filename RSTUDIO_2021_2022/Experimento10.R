partidos=read.csv("~/Escritorio/RSTUDIO/futbolNEW.csv",header=TRUE,sep=",")

#Añadir tres partidos a la lista: (10,0), (11,8) y (10,9)
nuevaFila=data.frame(Casa=c(10,11,10),Visitante=c(0,8,9))
partidos=rbind(partidos,nuevaFila)
tail(partidos)

#Calcular un nuevo vector a añadir al dataframe. Factor que sea "1","X","2" según el resultado.
partidos$Quinielas=(partidos$Casa>partidos$Visitante) * 1 + (partidos$Casa<partidos$Visitante) * 2
partidos$Quinielas=as.factor(partidos$Quinielas)
levels(partidos$Quinielas)=c("X","1","2")

#Tabla de frecuencia absoluta:
t1=table(partidos)
t1

t2=ftable(partidos,col.vars=c("Quinielas"))
t2

#Encontrar la tabla de frecuencias absolutas de los partidos empatados:
t3=apply(t1[,,"X"],MARGIN=1,FUN=sum)
t3
t3[c("0","1","2","3","4","5","6","7")]

#¿Cuántos partidos se empataron en total?
partidosempate=data.frame(t1[,,"X"])
sum(partidosempate$Freq)

#Gráfica de los partidos que han ganado los visitantes: 
visitantesgana=apply(t1[,,"2"],MARGIN=2,FUN=sum)
visitantesgana

maximo=max(visitantesgana)+30
grafica1=barplot(visitantesgana,
                 main = "Resultados visitante gana",
                 col=barplot(rep(1,12), col = cm.colors(12)),
                 legend.text = c("Goles visitante"), 
                 args.legend = list(x=13,y=400, cex=1), 
                 xlab="Goles visitante", ylab = "Frecuencia", ylim=c(0,maximo))
text(x=grafica1,y=visitantesgana+15,labels=visitantesgana)

#Gráfica de los partidos que se han empatado:
partidosempatados=apply(t1[,,"X"],MARGIN=2,FUN=sum)
partidosempatados

maximo=max(partidosempatados)+30
grafica1=barplot(partidosempatados,
                 main = "Resultados partidos empatados",
                 col=topo.colors(12,alpha = 1, rev = FALSE),
                 legend.text = c("Goles de ambos equipos"), 
                 args.legend = list(x=13,y=125, cex=0.75), 
                 xlab="Goles visitante", ylab = "Frecuencia", ylim=c(0,maximo))
text(x=grafica1,y=partidosempatados+15,labels=partidosempatados)


