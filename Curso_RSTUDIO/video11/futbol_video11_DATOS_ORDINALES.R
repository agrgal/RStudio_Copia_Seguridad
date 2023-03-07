# Ejercicio 9. Problema Fútbol
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

# Añado el average
futbol$Average = (futbol$Casa>futbol$Visitante)*(futbol$Casa-futbol$Visitante)
# Diferencia si han ganado los de casa, y cero en caso contrario
liga = futbol[,c(3,4)]

# Ordenar los factores
liga$Quiniela = ordered(liga$Quiniela,levels=c("1","X","2"))
liga$Average = ordered(liga$Average)
str(liga)
cat("\014")

# Obtener la tabla de frecuencias absolutas acumuladas del data frame liga.
# ¿Cuántos partidos han ganado los de casa?
t1=table(liga)
t1ACU = t(apply(t1,MARGIN=1,FUN=cumsum))
t1ACU
sum(t1["1",]) # otra forma de verlo

# Obtener la tabla de frecuencias relativas SIN ACUMULAR del data frame liga.
# ¿Qué es más probable, que el equipo empate o que gane 1-0? ¿En qué porcentaje?
t2=prop.table(table(liga))*100
round(t2,2)

# Obtener la tabla de frecuencias relativas ACUMULADAS del data frame liga.
# ¿Qué es más probable, que el equipo gane o pierda?
t2ACU=t(apply(round(t2,2),MARGIN=1,FUN=cumsum))
t2ACU
sprintf("Es más probable que gane por un margen del %.2f%%",t2ACU["1","10"]-t2ACU["2","10"] )

# ¿Qué es más probable, que el equipo pierda o que gane por menos
# de 4 goles de diferencia? Indica los datos de porcentaje.
sprintf("Es más probable que pierda por un margen del %.2f%%",
        t2ACU["2","10"]-t2ACU["1","3"] )

barplot(t1ACU,beside=TRUE, legend.text = levels(unique(liga$Quiniela))
        ,col=topo.colors(length(unique(liga$Quiniela)))
        ,args.legend=list(y=1500,x=10,cex=0.55,ncol=2)
        ,ylim=c(0,max(t1ACU)+200))
