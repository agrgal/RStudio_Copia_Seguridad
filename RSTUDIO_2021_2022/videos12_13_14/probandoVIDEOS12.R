rios = rivers
rios

# intervalos = c(0,500,1000,Inf)
intervalos=seq(0,max(rios)+40,length.out=4)

catRios = cut(rios, breaks=intervalos, right=FALSE, labels=c("Cortos","Medios","Largos"))
catRios
str(catRios)