  #Apartado 1
  partidos=read.csv("~/Escritorio/ R/Curso_RSTUDIO/video10/futbolNEW.csv",header=TRUE,dec=",")
  dim(partidos)
  # Apartado 2
  #Añadimos tres partidos a la lista:(10,0), (11,8),(10,9)
  nuevafila=data.frame(Casa=c(10,11,10), Visitante=c(0,8,9))
  partidos=rbind(partidos,nuevafila)
  tail(partidos)
  #Apartado 3
  #añadimos el vector quinielas
  partidos$quinielas=(partidos$Casa>partidos$Visitante)*1+
    (partidos$Casa<partidos$Visitante)*2
  partidos$quinielas=as.factor(partidos$quinielas)
  levels(partidos$quinielas)=c("X","1","2")
  #Apartado 4
  #tabla de frecuencias absolutas
  Casa=partidos$Casa
  Visitante=partidos$Visitante
  Quinielas=partidos$quinielas
  t1=ftable(Quinielas,Casa,Visitante,col.vars = "Quinielas")
  t1
  #Apartado 5
  cat("\014")
  #tabla de frecuencias absolutas de los partidos empatados
  #mediante Margin=c(1,2) obtenemos la matriz bidimensional
  #a partir de la anterior
  t2=table(Quinielas,Casa,Visitante)
  t3 =apply(t2, MARGIN = c(1,2), FUN = sum)
  t3
  # Partidos empatados
  t3["X",]
  #Eliminamos las columnas de valor 0, de la posicion 9 a la 12
  t3["X",-9:-12]
  #t3[, -9:-12]
  #Apartado 6
  #Resumen total segun los valores del los resultados de los partidos
  summary(partidos$quinielas)
  #Total partidos empatados
  sum(t3["X",-9:-12])
  #Apartado 7
  #Partidos en los que gana el visitante
  cat("\014")
  Gráfica=barplot(t3["2",-9:-12] ,main="Partidos que gana Visitante",beside=TRUE,legend.text = c("Gana
visitante a 0","Gana visitante a 1","Gana visitante a 2","Gana visitante a 3","Gana visitante a 4","Gana
visitante a 5","Gana visitante a 6","Gana visitante a 7"),args.legend =
                    list(x=10,y=400,cex=1),xlab="Número de goles",ylab="Partidos",col=c(topo.colors(14)))
  # Apartado 8
  Gráfica=barplot(t3["X",-9:-12] ,main="Partidos que empatan",beside=TRUE,legend.text = c("Empate a
0","Empate a 1","Empate a 2","Empate a 3","Empate a 4","Empate a 5","Empate a 6","Empate a
7"),args.legend = list(x=10,y=100,cex=1),xlab="Número de goles",ylab="Partidos",col=c(topo.colors(14)))
  