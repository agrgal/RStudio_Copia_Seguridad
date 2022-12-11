# =======================
# librerías y condiciones
# =======================
source("~/R/propuesta_mejora_Moodle/datos.R")

# ==========================
# Extrayendo datos
# ==========================

uCuaderno[uCuaderno$Valor=="No. Uso otros programas de registro como adittio, idoceo, etc.",]$Valor="No. Uso otros programas de registro\ncomo adittio, idoceo, etc."
df=data.frame(table(uCuaderno$Valor))
names(df)=c("Respuesta","frecuencia")
df=cbind(df,porcentajes=round(df$frecuencia/sum(df$frecuencia),2))


# ==============================
# A) Uso del cuaderno de Séneca
# ===============================

ggplot(df,aes(x=Respuesta, y=frecuencia, fill=Respuesta)) + # datos y aestética
          geom_bar(width = 0.75, stat='identity', colour="black") + # geometría básica de barra
          scale_fill_brewer(palette="Spectral", name="Uso del cuaderno de Séneca") +  # o bien directamente
          # scale_fill_hue(c=100,l=50) + #valores de cromaticidad y luminancia
          xlab("Respuestas")+ylab("Número de respuestas") + # etiquetas X e Y
          labs(title="¿Usas el cuaderno de Séneca?")+ # título
          theme(plot.title = element_text(lineheight=.8, # título
                face="bold", hjust="0.5",size=28,color="#4E0363",family="comic",
                margin=margin(t=10,b=5)))+
          scale_x_discrete(labels=abbreviate)+ # abreviar datos en eje X
          scale_y_continuous(breaks=seq(0,20,2))+ # formato de procentaje
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
          # theme(axis.ticks=element_line(size=4,color="#241484"))+ # Marcas: color y tamaño
          theme(axis.ticks=element_blank())+ # Quito las marcasdel eje X
          theme(legend.position = "top",legend.spacing.x = unit(0.5,"cm"))+
          # theme(legend.position = c(0.65,0.85),legend.spacing.x = unit(0.5,"cm"))+
          #posición de la leyenda y espaciado
          theme(legend.title=element_blank())+ # Quito el nombre de la leyenda
          theme(legend.text = element_text(size=8,colour="#241484",face="bold"))+ #tipo de texto
          theme(legend.background =
                element_rect(fill="#E8F5FB",colour="#4E0363",size=2))+ # caja de la leyenda
          theme(legend.margin =margin(r=10,l=5,t=5,b=5)) +
          guides(fill=guide_legend(ncol=2)) + # Distribuyo la leyenda en dos columnas
          theme(axis.text.x=
                  element_text(colour="#241484",size=16,face="bold"))+ # tamaño letra eje X
          theme(axis.text.y=
                  element_text(colour="#241484",size=16,face="bold")) + # tamaño letra eje Y
          theme(panel.grid.minor = element_blank(),panel.grid.major.x = element_blank()) +
          # voy a quitar los grids
          annotate("text",x=df$Respuesta,y=df$frecuencia/2
                   ,label=paste0(100*df$porcentajes,"%")
                   ,cex=8,col="#241484",fontface="bold",family="comic" #fontface="bold"
                  )



