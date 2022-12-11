Partidos = read.csv("futbolNEW.csv", header = TRUE)
Partidos = rbind(Partidos,  data.frame(Casa=c(10, 11, 10),Visitante=c(0, 8, 9)))
Partidos$Quiniela= ((Partidos$Casa>Partidos$Visitante) + (Partidos$Casa<Partidos$Visitante)*2) 
Partidos$Quiniela= as.factor(Partidos$Quiniela)
Partidos$Quiniela= factor(Partidos$Quiniela, labels = c("X","1","2"))
ventajagoles = c(Partidos$Casa-Partidos$Visitante)
derrotascero = c(ventajagoles>0)
average = c(derrotascero*ventajagoles)
liga = data.frame(Partidos$Quiniela, average)
liga$average= as.factor(liga$average)
liga$average=sort(liga$average,decreasing = FALSE)
liga$Partidos.Quiniela = ordered(liga$Partidos.Quiniela, levels=c("X","2","1"))
liga$Partidos.Quiniela= sort(liga$Partidos.Quiniela , decreasing = FALSE)
str(liga)
absolutacumulada=t(apply(table(liga), MARGIN = 1, FUN = cumsum)) 
absolutacumulada #1238 partidos ganados en casa
relativa = prop.table(table(liga))*100 
relativa #Hay un 16,7% de empate y un 14,6% de victoria de casa con un gol
t(apply(relativa, MARGIN = 1, FUN = cumsum)) #43% de que gane contra 40%de que pierda.
#Es mas probable que pierda, con un 40%, a con una diferencia de menos de 4 goles, con un 33,8%.

barplot(absolutacumulada, col=rainbow(7), ylim = c(0,3500),
        main = "Ventaja de goles en casa acumulada", xlab="Ventaja de goles(acumulada)",
        ylab= "Número de partidos",
        legend.text = c("Empate","Victoria visitante",
                                           "Victoria casa"),
        args.legend = list(x = "top"))

