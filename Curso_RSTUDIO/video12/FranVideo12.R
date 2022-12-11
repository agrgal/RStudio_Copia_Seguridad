#Cargar el dataframe con los valores del fichero adjunto.
parametros=read.csv("environment-indicators-for-spain-1.csv",header=TRUE,sep=",")
#Analiza el dataframe usando str.
str(parametros)
#Extrae los datos cuyo indicador de nombre sea "CO2 emissions from solid fuel
# consumption (kt)".
# co2=parametros$Value[parametros$Indicator.Name=="CO2 emissions from solid fuel consumption (kt)"]

parametrosElegidos = parametros[parametros$Indicator.Name=="CO2 emissions from solid fuel consumption (kt)",]
co2 = parametrosElegidos$Value
co2
str(co2)
#Utiliza la orden cut para obtener un vector categorizable del vector co2. Dentro de la orden
# cut, en el parámetro break, calcula un vector desde el mínimo de CO2 hasta el máximo de
# CO2 más uno, con cinco valores igualmente espaciados. Poner de etiquetas bajo, medio, alto
# y muy alto.
catco2=cut(co2, breaks = seq(min(co2), max(co2)+1, length.out=5), labels = c("bajo",
"medio", "alto", "muy alto"), right = FALSE)
catco2
#Conseguir que sea un factor ordenado.
catco2=ordered(catco2,levels=c("bajo","medio", "alto", "muy alto"))
catco2
str(catco2)
#Dibujar una gráfica barplot del factor ordenado. Frecuencias absolutas.
t1=table(catco2)
t1
maximo=max(t1)+15
grafica1=barplot(t1,
main = "Valores de CO2",
col=topo.colors(length(t1),alpha = 1, rev = FALSE),
legend.text = c("Niveles de CO2"),
args.legend = list(x=5,y=40, cex=1),
xlab="Clasificación de valores", ylab = "Frecuencia", ylim=c(0,maximo))
text(x=grafica1,y=t1+3,labels=t1)
#Dibujar una gráfica barplot del factor ordenado. Frecuencias relativas.
t2=round(100*prop.table(t1),2)
t2
maximo=max(t2)+15
grafica1=barplot(t2,
main = "Valores de CO2 en porcentaje",
col=heat.colors(length(t2), alpha = 1, rev = FALSE),
legend.text = c("Niveles de CO2 (%)"),
args.legend = list(x=5,y=63, cex=1),
xlab="Clasificación de valores", ylab = "Frecuencia", ylim=c(0,maximo))
text(x=grafica1,y=t2+3,labels=paste0(t2,"%"))