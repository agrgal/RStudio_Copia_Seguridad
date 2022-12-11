# Ejercicio 10. Múltiple valores
# =============================

futbol = read.csv("futbolNEW.csv",header=TRUE)

# Añado los partidos
futbol = rbind(futbol,data.frame(Casa=c(10,11,10),Visitante=c(0,8,9)))
tail(futbol)

# Añado la columna QUNIELA
cat("\014")
futbol$Quiniela = ((futbol$Casa-futbol$Visitante)>0)*1+((futbol$Casa-futbol$Visitante)<0)*2
futbol$Quiniela = as.factor(futbol$Quiniela)
levels(futbol$Quiniela)=c("X","1","2")
head(futbol,10)
str(futbol)

# Frecuencias absoluta y relativa
t1=table(futbol)
t2= ftable(futbol,col.vars=c("Quiniela"))
t2

# Encuentra la tabla de frecuencias de los partidos empatados
cat("\014")
empatados = futbol[futbol$Quiniela=="X",]
t3=ftable(empatados$Casa)
t3
sum(t3)
# o bien
t4 = apply(table(empatados),MARGIN=c(1,3),FUN=sum)
t4
cumsum(t4)

# Gráfica de los partidos cuyo resultado es  0 - X, según los goles de los visitantes
partidos0X = table(futbol[futbol$Casa==0 & futbol$Visitante>0,2])
colores=topo.colors(length(partidos0X))
old.par=par()
par=(mar=c(0,0,0,0))
migrafica = barplot(partidos0X,main="Nº partidos 0-X ",beside=TRUE,xlab="X",ylab="Nº partidos",
                    horiz=FALSE,col=colores, ylim=c(0,max(partidos0X)+20),
                    legend.text =names(partidos0X),
                    args.legend = list(x=20,y=40,cex=0.5,horiz=TRUE,text.col="darkgreen"))
text(migrafica, partidos0X + 0.1 , partidos0X, cex=0.6,pos=3) 
par = old.par

# Gráfica de los partidos cuyo resultado es  0 - X, según los goles de los visitantes
partidosXX = t4
colores=topo.colors(length(partidos0X))
old.par=par()
par=(mar=c(0,0,0,0))
migrafica = barplot(partidosXX,main="Nº partidos X-X ",beside=TRUE,xlab="X",ylab="Nº partidos",
                    horiz=FALSE,col=colores, ylim=c(0,max(partidos0X)+20),
                    legend.text =names(t4),
                    args.legend = list(x=20,y=40,cex=0.5,horiz=TRUE,text.col="darkgreen"))
text(migrafica, partidosXX + 0.1 , partidosXX, cex=0.6,pos=3) 
par = old.par

