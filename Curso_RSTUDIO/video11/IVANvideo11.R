#1
PARTIDOS=read.csv2("futbolNEW.csv",header=TRUE,sep=",") 


#2
PARTIDOSFILANUEVA=data.frame(Casa=c(10,11,10),Visitante=c(0,8,9))
PARTIDOS= rbind(PARTIDOS,PARTIDOSFILANUEVA)

#3
PARTIDOS$QUINIELA = (PARTIDOS$Casa>PARTIDOS$Visitante) + 2*(PARTIDOS$Casa<PARTIDOS$Visitante) 
PARTIDOS$QUINIELA = factor(PARTIDOS$QUINIELA,labels =c("X","1","2"))
#4
Average =(PARTIDOS$Casa>PARTIDOS$Visitante)*(PARTIDOS$Casa-PARTIDOS$Visitante)
Liga=data.frame(PARTIDOS$QUINIELA,Average)

str(Liga)

t1=table(Liga)
t1
apply(t1,MARGIN=1,FUN=cumsum)

t2=100*prop.table(t1)
t2
apply(t2,MARGIN=1,FUN=cumsum)

FRECUENCIASABSOLUTAS=cumsum(t1["1",])
FRECUENCIASABSOLUTAS

MAXIMO=max(FRECUENCIASABSOLUTAS[,c(2)])+100
MAXIMO

GRAFICA=barplot(FRECUENCIASABSOLUTAS[,c(2)],main= "RESULTADOS", 
                col=barplot(rep(1,14), col = hsv(seq(0,1 - 1/14,length.out=14),1,1)),
                legend.text= c("PARTIDOS"),
                args.legend= list(x=2, y=1500, cex=0.75),
                xlab="RESULTADOS", ylab= "F1", ylim=c (0,MAXIMO))


text(x=GRAFICA, y=FRECUENCIASABSOLUTAS[,c(2)]+50,labels= FRECUENCIASABSOLUTAS[,c(2)])


