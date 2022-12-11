# =======================
# Llama a otro script
# =======================

source("~/R/propuesta_mejora_Moodle/datos.R")
# ============================================
# A) Gráfica de participación por departamento
# ============================================

dpto=as.factor(dpto) #Convierto en factor
dptoPorcentajes=prop.table(table(dpto)) #Porcentajes de la tabla. En la gráfica se ponen en porcentajes después
df = data.frame(dptoPorcentajes)
# mipaleta=brewer.pal(length(df$dpto),"Spectral") #Elijo el mismo número que la longitud del vector

# =====================================================
# B) Gráfica de participación por departamento (ggplot)
# =====================================================
library(plotrix)
par(
  bg="lightblue",col.main="darkblue"
  , font.main=2, cex.main=2, family="comic"
  #color de fondo; colores, tipo de letra, tamaño del título
)
plot(1:10,type="n",main="Departamentos",xlab="",ylab="",axes=FALSE)
# plot vacío. Depende de sus elementos, puedo cantrar o no el pie
# box(col="red")
floating.pie(3,5,df$Freq,radius=1.5, #
             col=brewer.pal(length(df$dpto),"Spectral"),
             border="brown", #color del border
             lty=1, # tipo de línea del borde
             # density=4, edges=40,
             # angle=25, #Parece que no admite valores uqe no sean fijos
             # shadow=TRUE,
             # shadow.col=rev(brewer.pal(length(df$dpto),"Spectral"))
             explode=0.1 # puede ser un número fijo o un vector
)
pie.labels(x=3,y=5,
           labels = paste0(df$dpto," ",round(df$Freq*100,1),"%"),
           angles=(cumsum(df$Freq)-(df$Freq/2))*2*pi
           ,col="darkblue",font=2,
           cex=1,radius=1.85, # tamaño del label junto con el radio.
)
legend(x=8.5,y=5.5,paste0(df$dpto," ",round(df$Freq*100,1),"%") # labels
       ,fill=brewer.pal(length(df$dpto),"Spectral")
       ,cex=1, ncol=1 # tamaño de letras y número columnas, o bien horiz=TRUE
       ,bg="#C4CACC" #color de fondo
       ,box.lty=1,box.col = "darkblue",box.lwd = 2  #box.lty=0 o bty="n" para quitarlo
       # si tenemos una posición fija como top, right, etc. se puede usar inset
       # ,y.intersp=2,x.intersp = 0.1 # espacios entre las líneas
       ,text.width=3 # el ancho de la caja de texto
       ,xjust=0.5,yjust=0.5 #cuadro centrado según x e y.
       ,title="% por Departamentos"
)

