# =======================
# librerías y condiciones
# =======================
source("~/R/propuesta_mejora_Moodle/datos.R")

# ==========================
# Extrayendo datos
# ==========================

# ============================================
# A) Gráfica de participación por departamento
# ============================================

dpto=as.factor(dpto) #Convierto en factor
dptoPorcentajes=prop.table(table(dpto)) #Porcentajes de la tabla. En la gráfica se ponen en porcentajes después
df = data.frame(dptoPorcentajes)
# mipaleta=brewer.pal(length(df$dpto),"Spectral") #Elijo el mismo número que la longitud del vector

colores=colorRampPalette(brewer.pal(11, "Spectral"))(length(unique(dpto)))

ggplot(df,aes(x=dpto, y=Freq, fill=dpto))+ # datos y aestética
         geom_bar(width = 0.75, stat='identity', colour="black")+ # geometría básica de barra
        # scale_fill_brewer(palette="Spectral", name="Departamentos",type="qual") +  # o bien directamente
        scale_fill_manual(values=colores) +
         # scale_fill_hue(c=100,l=50) + #valores de cromaticidad y luminancia
         xlab("Departamentos")+ylab("Miembros que han contestado")+ # etiquetas X e Y
         labs(title="Participación en la encuesta")+ # título
         theme(plot.title = element_text(lineheight=.8, # título
               face="bold", hjust="0.5",size=28,color="#4E0363",family="comic",
               margin=margin(t=10,b=10)))+
         scale_x_discrete(labels=abbreviate)+ # abreviar datos en eje X
         scale_y_continuous(labels=percent)+ # formato de procentaje
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
         theme(legend.position = c(0.25,0.8),legend.spacing.x = unit(0.5,"cm"))+
         #posición de la leyenda y espaciado
         theme(legend.title=element_blank())+ # Quito el nombre de la leyenda
         theme(legend.text = element_text(size=8,colour="#241484",face="bold"))+ #tipo de texto
         theme(legend.background = 
               element_rect(fill="#E8F5FB",colour="#4E0363",size=2))+ # caja de la leyenda
         guides(fill=guide_legend(ncol=3)) + # Distribuyo la leyenda en dos columnas
         theme(axis.text.x=
                 element_text(colour="#241484",size=16,face="bold"))+ # tamaño letra eje X
         theme(axis.text.y=
                 element_text(colour="#241484",size=16,face="bold")) + # tamaño letra eje Y
annotate("text",x=df$dpto,y=df$Freq/2
         ,label=paste0(round(100*df$Freq,1),"%")
         ,hjust=0.5,vjust=0.5
         ,cex=5,col="#241484",fontface="bold",family="comic",angle=0 #fontface="bold"
) 
# =====================================================
# B) Gráfica de participación por departamento (ggplot)
# =====================================================


             
             

