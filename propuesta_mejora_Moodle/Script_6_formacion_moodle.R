# =======================
# librerías y condiciones
# =======================
source("~/R/propuesta_mejora_Moodle/datos.R")

# ==========================
# Extrayendo datos
# ==========================
fMoodle$Valor=gsub("IES ","IES\n",fMoodle$Valor)
fMoodle$Valor=gsub("en ","en\n",fMoodle$Valor)
fMoodle$Valor=gsub("de ","de\n",fMoodle$Valor)
fMoodle$Valor=gsub(" por","\npor",fMoodle$Valor)
library("dplyr")
df=data.frame(table(fMoodle$Valor))
names(df)=c("Modificador","Frecuencia")

df$Modificador = as.character(df$Modificador) # cambio a character
ordenado = unique(df$Modificador) # obtengo los valores sin repetir
ordenado=c(ordenado[1],ordenado[2],ordenado[3],ordenado[5],ordenado[6],ordenado[4]) # pongo el orden que necesito
df$Modificador=factor(df$Modificador,levels=ordenado) # refactorizo con el orden que necesito

# ===========================
# A) Tipo de formación Moodle
# ===========================
ggplot(df,aes(x=reorder(Modificador,desc(Modificador)), y=Frecuencia,fill=Modificador)) + # datos y aestética
  geom_bar(width = 0.75, stat='identity', colour="black", position="dodge") + # geometría básica de barra
  coord_flip() + # cambio ejes x e y
  scale_fill_brewer(palette="Reds", name="Formación en Moodle") +
  scale_y_continuous(breaks=seq(0,max(df$Frecuencia),2)) +
  # o bien directamente
  # scale_fill_manual(values = rev(brewer.pal(4,"Set2"))
  #                  , name="Estado de la formación") + 
  # scale_fill_hue(c=100,l=50) + #valores de cromaticidad y luminancia
  xlab("Tipos de formación")+ylab("Número de respuestas") + # etiquetas X e Y
  labs(title="Formación en Moodle Centros")+ # título
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
  theme(axis.ticks=element_blank())+ # Quito las marcasdel eje X
  # theme(legend.position = "top",legend.spacing.x = unit(0.5,"cm"))+
  # # posición de la leyenda y espaciado
  # theme(legend.title=element_blank())+ # Quito el nombre de la leyenda
  # theme(legend.text = element_text(size=8,colour="#241484",face="bold"))+ #tipo de texto
  # theme(legend.background =
  #         element_rect(fill="#E8F5FB",colour="#4E0363",size=2))+ # caja de la leyenda
  # theme(legend.margin =margin(r=10,l=5,t=5,b=3)) +
  # theme(legend.key.size = unit(0.4,"cm")) +
  # guides(fill=guide_legend(nrow=2)) + # Distribuyo la leyenda en una fila
  guides(fill=FALSE)+ # No hace falta la leyenda
  theme(axis.text.x=
          element_text(colour="#241484",size=16,face="bold"))+ # tamaño letra eje X
  theme(axis.text.y=
          element_text(colour="#241484",size=11,face="bold",hjust=0)) + # tamaño letra eje Y
  theme(panel.grid.minor = element_blank(),panel.grid.major.x = element_blank())+
  annotate("text",x=df$Modificador,y=df$Frecuencia/2
            ,label=paste0(df$Frecuencia)
            ,cex=8,col="black",fontface="bold",family="comic" #fontface="bold"
           )
