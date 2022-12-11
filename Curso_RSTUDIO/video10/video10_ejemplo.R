pediatria=read.csv("pediatria.csv",header=TRUE,dec=",")

str(pediatria)

Edad=pediatria$Edad
Peso = pediatria$Peso
Sexo = pediatria$Sexo
rm(pediatria)

t1=table(Edad,Peso,Sexo)
t2=ftable(Edad,Peso,Sexo)
t1
t2

t2=ftable(Edad,Peso,Sexo,col.vars = c("Sexo","Edad"))
t2

cat("\014")

t1["8",,"F"]
t2["9","40",]

ftable(prop.table(t1)*100,col.vars=c("Sexo","Edad"))


cat("\014")
ftable(round(prop.table(t1,margin=1)*100,2),col.vars=c("Sexo","Edad"))
round(100*prop.table(t1,margin=1),2)["11",,]
sum(round(100*prop.table(t1,margin=1),2)["11",,])


cat("\014")
ftable(round(prop.table(t1,margin=3)*100,2),col.vars=c("Sexo","Edad"))
round(100*prop.table(t1,margin=3),2)[,,"F"]
sum(round(100*prop.table(t1,margin=3),2)[,,"F"])

cat("\014")
ftable(round(prop.table(t1,margin=c(1,3))*100,2),col.vars=c("Sexo","Edad"))
round(100*prop.table(t1,margin=c(1,3)),2)["11",,"F"]
sum(round(100*prop.table(t1,margin=c(1,3)),2)["11",,"F"])

t3=apply(t1,MARGIN=c(1,2),FUN=sum)
t3

cat("\014")
tfrMismoSexoEdad=(prop.table(t1,margin=c(1,3))*100)["11",,"F"]
pesos=sort(unique(Peso))
pesos
colores=topo.colors(length(pesos))
colores
leyenda=paste("Peso ",pesos,sep=" ")
leyenda

old.par=par()
par=(mar=c(0.5,0.5,0.5,0.5))
migrafica=barplot(tfrMismoSexoEdad,
                  main="frecuencias relativas",
                  beside=TRUE,
                  horiz=FALSE,
                  col=colores,
                  ylim=c(0,max(tfrMismoSexoEdad)+2),
                  legend.text=leyenda,
                  args.legend=list(title="leyenda PESOS",
                                   x=10,y=11,cex=0.5,horiz=FALSE,
                                   ncol=4,text.col="darkgreen")
                  )
text(migrafica,0,round(tfrMismoSexoEdad,1),cex=1,pos=3)
text(migrafica,tfrMismoSexoEdad+0.1,round(tfrMismoSexoEdad,1),cex=1,pos=3)
par=old.par