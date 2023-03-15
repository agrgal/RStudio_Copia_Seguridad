PartidosNuevos = read.csv("/home/aurelio/Escritorio/RSTUDIO/futbolNEW.csv" ,header=TRUE, sep=",")
PartidosNuevos

#AÑADIR PARTIDOS (10,0)(11,8)(10,9)

Fila=data.frame(local=c(10,11,10), Visitante=c(0,8,9))
PartidosNuevos=rbind(PartidosNuevos,Fila)
tail(PartidosNuevos)

#VECTOR, FACTOR= 1,X,2
PartidosNuevos$Quinielas=(PartidosNuevos$Casa>PartidosNuevos$Visitante)* 1 + (PartidosNuevos$Casa<PartidosNuevos$Visitante) * 2
PartidosNuevos$Quinielas=as.factor(PartidosNuevos$Quinielas)
levels(PartidosNuevos$Quinielas)=c("X","1","2")

#FRECUENCIAS ABSOLUTAS TABLA 1
T1=table(PartidosNuevos)
T1
#FRECUENCIAS ABSOLUTAS TABLA 2
T2=ftable(PartidosNuevos,col.vars=c("Quinielas"))
T2
#FRECUENCIAS ABSOLUTAS TABLA 3
T3=apply(T1[,,"X"],MARGIN=1,FUN=sum)
T3
T3[c("0","1","2","3","4","5","6","7")]
   
PartidosEmpate=data.frame(T1[,,"X"])
sum(PartidosEmpate$freq)

#PARTIDOS GANAN VISITANTES
Visitantesganan=apply(T1[,, "2"],MARGIN=2, FUN=sum)
Visitantesganan

#GRÁFICA RESULTADOS VISITANTE GANA
maximo=max(Visitantesganan)+30
grafica1=barplot(Visitantesganan,
                 main = "RESULTADOS DONDE VISITANTE GANA",
                 col=barplot(rep(1,12), col=cm.colors(8)), 
                 legend.text = c("Gol Visitante"),
                 args.legend = list(x=10.5,y=400,cex=1),
                 xlab="GOL VISITANTE", ylab = "FRECUENCIA", ylim=c(0,maximo))
text(x=grafica1,y=Visitantesganan+15,labels=Visitantesganan)

#PARTIDOS EMPATADOS
Partidosempatados=apply(T1[,,"X"],MARGIN=2,FUN=sum)
Partidosempatados

#GRÁFICA PARTIDOS EMPATADOS
maximo=max(Partidosempatados)+30
grafica1=barplot(Partidosempatados,
                 main = "RESULTADOS PARTIDOS EMPATADOS",
                 col=topo.colors(12,alpha=1,rev=FALSE), 
                 legend.text = c("Goles de los 2 Equipos"),
                 args.legend = list(x=10,y=125,cex=1),
                 xlab="GOL VISITANTE", ylab = "FRECUENCIA", ylim=c(0,maximo))
text(x=grafica1,y=Partidosempatados+15,labels=Partidosempatados)