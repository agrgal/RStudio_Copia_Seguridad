partidos = read.csv("~/Escritorio/RSTUDIO/futbolNEW.csv", header = TRUE, dec=",")

partidosExtras = data.frame(Casa=c(10,11,10),Visitante=c(0,8,9))
partidos = rbind(partidos,partidosExtras)

partidos$Quiniela = c((partidos$Casa>partidos$Visitante)+(partidos$Casa<partidos$Visitante)*2)
partidos$Quiniela = as.factor(partidos$Quiniela)
partidos$Quiniela = factor(partidos$Quiniela ,labels = c("x","1","2"),)

partidos$Avarage = (partidos$Casa>partidos$Visitante)*(partidos$Casa - partidos$Visitante)

liga = data.frame(partidos$Quiniela,partidos$Avarage)

liga = liga[order(liga$partidos.Quiniela,liga$partidos.Avarage),]
str(liga)

t1 = t(table(liga))
t2 = apply(t1, MARGIN = 2, FUN = cumsum)
t3 = ((prop.table(t1))*100)
t4 = cumsum(t3)

barplot(t2, col = rainbow(10),
        main="Frecuencias absolutas acumuladas", 
        xlab="Posibles resultados", 
        ylab="Número de partidos")