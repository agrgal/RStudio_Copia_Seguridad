# =======================
# librerías y condiciones
# =======================
source("~/R/propuesta_mejora_Moodle/datos.R")

# ==========================
# Extrayendo datos
# ==========================
fSeneca$Modificador=gsub("en ","en\n",fSeneca$Modificador)
fSeneca$Valor=gsub("en ","en\n",fSeneca$Valor)
library("dplyr")
df=data.frame(table(fSeneca[,1:2]))

df$Modificador = as.character(df$Modificador) # cambio a character
ordenado = unique(df$Modificador) # obtengo los valores sin repetir
ordenado=c(ordenado[2],ordenado[1],ordenado[3]) # pongo el orden que necesito
df$Modificador=factor(df$Modificador,levels=ordenado) # refactorizo con el orden que necesito

# =====================
# A) Tipo de formación
# =====================
ggplot(data=df, aes(y=Freq, x=Modificador,color=Valor,shape=Valor,group=Valor)) +
  geom_point(size=8) +
  geom_line(size=1.5) +
  scale_shape_manual(values=c(15,16,18,17)) +
  scale_color_manual(values = brewer.pal(4,"PuOr"))+
  # scale_fill_manual(values = rev(brewer.pal(4,"Greens"))
                    # , name="Estado de la formación") +
  scale_y_continuous(breaks=seq(0,max(df$Freq),2),limits=c(0,max(df$Freq))) +
  # scale_fill_hue(c=100,l=50) + #valores de cromaticidad y luminancia
  xlab("Tipos de formación")+ylab("Número de respuestas") + # etiquetas X e Y
  labs(title="Estado de la formación en Séneca")+ # título
  theme(plot.title = # título
          element_text(lineheight=.8, face="bold", hjust="0.5",size=24,color="#4E0363"
                       ,family="comic"  ,margin=margin(t=10,b=10)))+
  # scale_x_discrete(labels=abbreviate)+ # abreviar datos en eje X
  # scale_y_continuous(labels=percent)+ # formato de procentaje
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
  theme(axis.ticks.x=element_blank(),axis.ticks.y=element_blank() )+ #Quito las marcasdel ejeX
  theme(legend.position = "top",legend.spacing.x = unit(0.2,"cm"))+
  # posición de la leyenda y espaciado
  guides(fill=guide_legend(ncol=2)) + # Distribuyo la leyenda en una fila
  theme(legend.title=element_blank())+ # Quito el nombre de la leyenda
  theme(legend.text = element_text(size=8,colour="#241484",face="bold",hjust=0.5))+ #tipo de texto
  theme(legend.background =
          element_rect(fill="#E8F5FB",colour="#4E0363",size=2))+ # caja de la leyenda
  theme(legend.margin =margin(r=11,l=5,t=5,b=3)) +
  theme(legend.key.size = unit(0.1,"cm")) +
  theme(axis.text.x=
          element_text(colour="#241484",size=11,face="bold"))+ # tamaño letra eje X
  theme(axis.text.y=
          element_text(colour="#241484",size=13,face="bold",hjust=0.5)) + # tamaño letra eje Y
  theme(panel.grid.minor = element_blank(),panel.grid.major.x = element_blank(),
        panel.grid.major.y=element_line(color="darkblue"))