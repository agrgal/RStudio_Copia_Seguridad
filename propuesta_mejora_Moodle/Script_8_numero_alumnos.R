# =======================
# librerías y condiciones
# =======================
source("~/R/propuesta_mejora_Moodle/datos.R")

# ==========================
# Extrayendo datos
# ==========================
library("dplyr")
df=data.frame(table(enpNumero[,1:2]))
names(df)=c("Intervalos","Valor","frecuencia")

# Cambio valores
df$Valor=gsub("en ","\nen ",df$Valor)
df$Valor=gsub("de ","\nde ",df$Valor)
df$Valor=gsub(" C","\nC",df$Valor)
df$Valor=gsub(" dig","\ndig",df$Valor)

tdf = xtabs(frecuencia ~ Intervalos+Valor, data=df)
df=data.frame(prop.table(tdf,margin=2)) # acumulado en Intervalo
names(df)=c("Intervalos","Valor","frecuencia")
df$frecuencia=round(df$frecuencia,2)

# ===========================
# A) Plataformas digitales
# ===========================
ggplot(df,aes(x=Intervalos, y=frecuencia, fill=Valor))+ # datos y aestética
  geom_bar(width = 0.75, stat='identity', colour="black",position="dodge")+ # geometría básica de barra
  scale_fill_brewer(palette="Spectral", name="Número alumnos") +  # o bien directamente
  facet_wrap(~gsub("\n"," ",Valor))+
  # facet_wrap(~Valor)+
  # scale_fill_hue(c=100,l=50) + #valores de cromaticidad y luminancia
  xlab("Intervalo de número de alumnos/as")+ylab("Porcentaje de respuestas")+ # etiquetas X e Y
  labs(title="Distribución de plataformas por número de alumnos/as")+ # título
  theme(plot.title = element_text(lineheight=.8, # título
                                  face="bold", hjust="0.5",size=24,color="#4E0363"                                   ,family="comic",margin=margin(t=20,b=15)))+
  # scale_x_discrete(labels=abbreviate)+ # abreviar datos en eje X
  scale_y_continuous(labels=percent_format(accuracy=1))+ # formato de procentaje
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
  theme (panel.background = element_rect(fill = "#5397B4"))+ # fondo del área gráfica
  theme(axis.ticks=element_line(size=4,color="#241484"))+ # Marcas: color y tamaño
  theme(legend.position = c(0.85,-0.15),legend.spacing.x = unit(0.5,"cm"))+
  #posición de la leyenda y espaciado
  theme(legend.title=element_blank())+ # Quito el nombre de la leyenda
  theme(legend.text = element_text(size=8,colour="#241484",face="bold"))+ #tipo de texto
  theme(legend.background = 
          element_rect(fill="#E8F5FB",colour="#4E0363",size=2))+ # caja de la leyenda
  guides(fill=guide_legend(ncol=2)) + # Distribuyo la leyenda en dos columnas
  theme(panel.grid.minor = element_blank(),panel.grid.major.x=element_blank()) +
  theme(axis.text.x=
          element_text(colour="#241484",size=11,face="bold",angle=90))+ # tamaño letra eje X
  theme(axis.text.y=
          element_text(colour="#241484",size=16,face="bold")) +  # tamaño letra eje
  geom_text(data=df, x=df$Intervalos,y=df$frecuencia
           ,aes(label=paste0(100*frecuencia,"%"))
           ,hjust=+0.5,vjust=-0.8
           ,cex=3,col="#241484",fontface="bold",family="comic" #fontface="bold"
  ) +
  theme(strip.text.x=
          element_text(colour="#241484",size=11,face="bold",angle=0)) + # texto de los facets
  theme(strip.background = element_blank()) # color fondo
  