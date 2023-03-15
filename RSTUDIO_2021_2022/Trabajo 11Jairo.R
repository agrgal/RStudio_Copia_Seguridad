#Apartado 1
partido=read.csv("~/Escritorio/RSTUDIO/futbolNEW.csv" ,header=TRUE,dec=",")
#Apartado 2 
nuevaFila=data.frame(Casa=c(10,11,10), Visitante=c(0,8,9))
partido=rbind(partido,nuevaFila)
#Apartado 3
partido$quiniela=(partido$Casa>partido$Visitante)+2*(partido$Casa<partido$Visitante)
partido$quiniela=factor(partido$quiniela)
x12=levels(partido$quiniela)=c("X","1","2")
str(partido)
#Apartado 4 
partido$average= (partido$Casa>partido$Visitante)*(partido$Casa-partido$Visitante)
#Apartado 5
liga=partido[,c(3,4)]
#Apartado 6
liga$average=ordered(liga$average)
str(liga)
liga$quiniela=ordered(liga$quiniela,levels=c("1","X","2"))
str(liga)
# Apartado 7
t1=t(table(liga))
t2=apply(t1, MARGIN = 2, FUN = cumsum)
#Apartado 8
t3=t(prop.table(table(liga))*100)
t3=round(t3,2)
#Apartado 9
t4=apply(t3,MARGIN = 2, FUN = cumsum)
#Apartado 10
barplot(t2,beside = TRUE,
        col = cm.colors(10),
        args.legend=list(x=0.4,y=290,cex=1),xlab="quiniela",ylab="goles",ylim=c(0,1200),
        main = "Grafica de goles en la quiniela")