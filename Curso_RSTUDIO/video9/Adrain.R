Partidos = read.csv("futbol.csv", header = TRUE)
dim(Partidos) #2870 Partidos
Partidos = rbind(Partidos, data.frame(Casa=c(10, 11, 10),Visitante=c(0, 8, 9)))
tail(Partidos) #Se han añadido correctamente
t1 = table(Partidos$Casa, Partidos$Visitante)
t1
100*prop.table(t1) # *100 para que de porcentajes
resultados = data.frame(t1)
resultados$Var1 = as.character(resultados$Var1)
resultados$Var2 = as.character(resultados$Var2)
str(resultados)
resultados = rbind(resultados , data.frame(Var1=c(9,9,9,9,9,9,9,9,9,9), Var2=c(0,1,2,3,4,5,6,7,8,9),
                                           Freq=c(0,0,0,0,0,0,0,0,0,0)))
# ordenar
resultados$Var1 = as.numeric(resultados$Var1)
resultados$Var2 = as.numeric(resultados$Var2)
resultados = resultados[order(resultados$Var2,resultados$Var1),]
# refactoriza
resultados$Var1 =as.factor(resultados$Var1)
resultados$Var2 = as.factor(resultados$Var2)
length(which(resultados$Freq==0)) #51 resultados
max(resultados$Freq) #96 partidos tienen el mismo resultado
# tabla recalculada
t1 = xtabs(Freq~Var1+Var2,data=resultados)
t3=apply(t1,MARGIN=1,FUN=sum)
t4 = prop.table(t3)*100
t4["0"]
resultados[resultados$Freq==96,] #El resultado más frecuente es 2-1
casa0=prop.table(t1)[,"0"]*100
casa0
sum(casa0)#Hay un 16,6% que marque 0 goles el equipo de casa
vis0=prop.table(cas0t1)["0",]*100
vis0
sum(vis0)#Hay un 16,7% que marque 0 goles el equipo visitante
barplot(casa0, col=topo.colors(length(casa0)),ylim=c(0,3),main="Resultados con 0 goles en casa", ylab
        ="Frecuencia Relativa")
ganadosCasa = dim(Partidos[Partidos$Casa>Partidos$Visitante,])[1]/dim(Partidos)[1]*100#43%
ganadosVis = dim(Partidos[Partidos$Casa<Partidos$Visitante,])[1]/dim(Partidos)[1]*100#43%
empatados = dim(Partidos[Partidos$Casa==Partidos$Visitante,])[1]/dim(Partidos)[1]*100#43%
winrate= c(ganadosCasa,ganadosVis,empatados)
barplot(prop.table(winrate)*100 , col=c("green","red","cyan"), ylim=c(0,100),
        main = "Indice de victoria", ylab="Frecuencia relativa en %",
        legend.text=c("Victoria casa","Victoria visitante","Empate"))
######################################################