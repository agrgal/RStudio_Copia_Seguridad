#1
PARTIDOS=read.csv("futbolNEW.csv",header=TRUE,dec=",") 


#2
PARTIDOSFILANUEVA=data.frame(Casa=c(10,11,10),Visitante=c(0,8,9))
PARTIDOS= rbind(PARTIDOS,PARTIDOSFILANUEVA)

#3
PARTIDOS$QUINIELA = (PARTIDOS$Casa>PARTIDOS$Visitante) + 2*(PARTIDOS$Casa<PARTIDOS$Visitante) 
PARTIDOS$QUINIELA = factor(PARTIDOS$QUINIELA,labels =c("X","1","2"))

#4
str(PARTIDOS)
ftable(PARTIDOS$Casa, PARTIDOS$Visitante, PARTIDOS$QUINIELA)
t1=table(PARTIDOS$Casa, PARTIDOS$Visitante, PARTIDOS$QUINIELA)
t1

#5
t2=apply(t1[,,"X"],MARGIN=1,FUN=sum)
sum(t2)

#6
rainbow(t2)
barplot(t2)
barplot((t2),col=c("#00FF30FF","#0066FFFF","#96FF00FF","#FFEA00FF", "#00FF0CFF","#3CFF00FF","#0600FFFF"))
PARTIDOS$Visitante= PARTIDOS[which(PARTIDOS$QUINIELA=="2"),]
t3= table(PARTIDOS$Casa, PARTIDOS$Visitante)
t3
t4= apply(t3,MARGIN= 1,FUN =sum)
t4
barplot(t4)
barplot((t4),col=c("#00FF30FF","#0066FFFF","#96FF00FF","#FFEA00FF", "#00FF0CFF","#3CFF00FF","#0600FFFF"))
