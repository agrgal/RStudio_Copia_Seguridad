# =======================
# librerías y condiciones
# =======================
source("~/R/propuesta_mejora_Moodle/datos.R")

# ==========================
# Extrayendo datos
# ==========================
library("dplyr")
df=data.frame(table(enpClases[,2:3]))
names(df)=c("Valor","Departamento","frecuencia")

# Ordenar datos según un vector en un orden dado
# match da el resultado, por cada valor de datos$Clase en qué lugar está del vector clases
# y order los ordena.
df$Valor=gsub("en ","\nen ",df$Valor)
df$Valor=gsub("de ","\nde ",df$Valor)
df$Valor=gsub(" C","\nC",df$Valor)
df$Valor=gsub(" dig","\ndig",df$Valor)

# convierto el data frame a tabla, con la función xtabs
tdf = xtabs(frecuencia ~ Departamento+Valor, data=df)
# y vuelvo a recalcular niveles como un data frame, pero ahora con frecuencias relativas
df=data.frame(prop.table(tdf,margin=1)) # acumulado en nivel
names(df)=c("Departamento","Valor","frecuencia")
df$frecuencia=round(df$frecuencia*100,1)
df2=data.frame(prop.table(tdf,margin=2)) # acumulado en tipo
names(df2)=c("Departamento","Valor","frec2")
df2$frec2=round(df2$frec2*100,1)

df
df2

# B1) 

# ===========================
# A) Plataformas digitales
# ===========================
colores=colorRampPalette(brewer.pal(9, "OrRd"))(34)

ggplot(df,aes(x=Departamento, y=Valor)) +
  scale_color_manual(values = colores, guide=FALSE)+
  geom_point(data=df,aes(color=as.factor(frecuencia),size=frecuencia),alpha=0.9)+
  geom_point(data=df,aes(size=frecuencia),shape=21,colour="black",stroke=1.5) +    #añade bordes negros
  scale_size(range = c(0, 10), name="Frecuencia") +
  guides(size=guide_legend(override.aes=list(colour=colores[seq(1,24,length.out=5)]))) + # cambia los colores de la leyenda
  xlab("Departamentos")+ylab("% de respuestas") + # etiquetas X e Y
  labs(title="Uso de distintas plataformas digitales - Departamentos(%)")+ # título
  theme(plot.title = element_text(lineheight=.8, # título
                                  face="bold", hjust="0.5",size=24,color="#4E0363"                       ,family="comic",margin=margin(t=10,b=10))) +
  scale_x_discrete(labels=abbreviate)+ # abreviar datos en eje X
  # # scale_y_continuous(labels=percent)+ # formato de procentaje
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
  # guides(color=brewer.pal(max(df$frecuencia)+1,"Oranges"))+
  theme(legend.position = "right", legend.spacing.x = unit(0.5,"cm"))+
  # posición de la leyenda y espaciado
  theme(legend.title=element_blank())+ # Quito el nombre de la leyenda
  theme(legend.text = element_text(size=8,colour="#241484",face="bold"))+ #tipo de texto # E8F5FB
  theme(legend.background =
          element_rect(fill="#E8F5FB",colour="#4E0363",size=2))+ # caja de la leyenda
  theme(legend.margin =margin(r=10,l=5,t=5,b=5)) +
  theme(legend.box.just = "top", legend.box.margin = margin(l=40,r=20))+
  # guides(fill=guide_legend(nrow=1)) + # Distribuyo la leyenda en una fila
  theme(axis.text.x=
          element_text(colour="#241484",size=10,face="bold",angle=0,vjust=0.5))+ # tamaño letra eje X y vjust para centrar
  theme(axis.text.y=
          element_text(colour="#4E0363",size=11,face="bold",hjust=0.5)) + # tamaño letra eje Y
  theme(panel.grid.minor = element_blank()) +
  annotate("text",x=df$Departamento,y=df$Valor
           ,label=paste0(df$frecuencia,"%")
           ,hjust=1,vjust=-1.2
           ,cex=4,col="#241484",fontface="bold",family="comic" #fontface="bold"
  ) +
  annotate("text",x=df$Departamento,y=df$Valor
           ,label=paste0(df2$frec2,"%")
           ,hjust=-0.4,vjust=+3 #4E0363
           ,cex=3,col="#B31B71",fontface="bold",family="comic" #fontface="bold"
  )