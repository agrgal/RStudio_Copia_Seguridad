# =======================
# librerías y condiciones
# =======================
source("~/R/propuesta_mejora_Moodle/datos.R")

substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}

# ==========================
# Extrayendo datos
# ==========================
library("dplyr")

# Cambio valores
enpNumero$Valor=gsub("en ","\nen ",enpNumero$Valor)
enpNumero$Valor=gsub("de ","\nde ",enpNumero$Valor)
enpNumero$Valor=gsub(" C","\nC",enpNumero$Valor)
enpNumero$Valor=gsub(" dig","\ndig",enpNumero$Valor)

enpNumero$num=substrRight(as.character(enpNumero$Modificador),3)
enpNumero[enpNumero$Modificador=="Más de 100",]$num="110"
enpNumero[enpNumero$num=="e 5",]$num="5"
enpNumero[enpNumero$num=="uno",]$num="0"
enpNumero$num=as.numeric(enpNumero$num)

library(plyr)
mu = ddply(enpNumero, "Valor", summarise, grp.mean=mean(num),grp.mediana=median(num))
mu

# NOTA: no lo convierto a tabla. Tal como está es como lo entiende 
# el dibujo del histograma

# convierto el data frame a tabla, con la función xtabs
# tdf = xtabs(frecuencia ~ num+Valor, data=df)
# tdf

ggplot(enpNumero,aes(x=num,fill=Valor))+
  geom_histogram(binwidth=10,alpha=0.5,color="black",aes(y = ..density..),position="identity")+
  geom_density(alpha=.2,fill="darkblue",color="darkblue")+
  geom_vline(data=mu,aes(xintercept=grp.mean),linetype="dashed", size=1,color="darkblue",alpha=0.5)+ # Señaliza la media
  geom_vline(data=mu,aes(xintercept=grp.mediana),linetype="dashed", size=1,color="red",alpha=0.5)+ # Señaliza la media
  facet_wrap(~Valor) +   # facet_grid(~Valor)
  # scale_size(range = c(0, 10), name="Frecuencia") +
  # guides(size=guide_legend(override.aes=list(colour=colores[seq(1,24,length.out=5)]))) + # cambia los colores de la leyenda
  xlab("Número de alumnos")+ylab("Probabilidad") +  # etiquetas X e Y
  labs(title="Histogramas de uso por num. alumnos/as") + # título
  theme(plot.title = element_text(lineheight=.8, # título
                      face="bold", hjust="0.5",size=24,color="#4E0363"                                   ,family="comic",margin=margin(t=10,b=10))) +
  # scale_x_discrete(labels=abbreviate)+ # abreviar datos en eje X
  # scale_y_continuous(labels=percent) # formato de procentaje
  theme(axis.title.y = element_text( # título del eje Y
     family="amano",size=30,face="bold",color="#241484",
     margin = margin(t = 0, r = 20, b = 0, l = 10)))+
  theme(axis.title.x = element_text( # título del eje X
     family="amano",size=30,face="bold",color="#241484",
     margin = margin(t = 20, r = 0, b = 10, l = 0)))+
  theme (plot.background = element_rect(fill = "#5397B4", # fondo total de la imagen
                                         colour = "#4E0363",
                                         size = 2, linetype = "solid"))+
   theme(plot.margin=margin(r=20))+ #margen (padding) del área total.
   theme(panel.background = element_rect(fill = "#5397B4"))+ # fondo del área gráfica
   
  theme(axis.ticks=element_line(size=4,color="#241484"))+ # Marcas: color y tamaño
  # theme(axis.ticks.x=element_blank())+ # Quito las marcasdel eje X
  # # guides(color=brewer.pal(max(df$frecuencia)+1,"Oranges"))+
  theme(legend.position = "none", legend.spacing.x = unit(0.5,"cm"))+
  # # posición de la leyenda y espaciado
  # theme(legend.title=element_blank())+ # Quito el nombre de la leyenda
  # theme(legend.text = element_text(size=8,colour="#241484",face="bold"))+ #tipo de texto # E8F5FB
  # theme(legend.background =
  #          element_rect(fill="#E8F5FB",colour="#4E0363",size=2))+ # caja de la leyenda
  # theme(legend.margin =margin(r=10,l=5,t=5,b=5)) +
  # theme(legend.box.just = "top", legend.box.margin = margin(l=40,r=20))+
  #  # guides(fill=guide_legend(nrow=1)) + # Distribuyo la leyenda en una fila
  theme(axis.text.x=
           element_text(colour="#241484",size=10,face="bold",angle=0,vjust=0.5))+ # tamaño letra eje X y vjust para centrar
  theme(axis.text.y=
           element_text(colour="#4E0363",size=11,face="bold",hjust=0.5)) + # tamaño letra eje Y
  theme(panel.grid.minor = element_blank(), panel.grid.major.x=element_blank()) +
theme(strip.text.x=
        element_text(colour="#241484",size=11,face="bold",angle=0)) + # texto de los facets
  theme(strip.background = element_blank())   # color fondo

  