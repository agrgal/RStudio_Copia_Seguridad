# =======================
# librerías y condiciones
# =======================
source("~/R/propuesta_mejora_Moodle/datos.R")

# ==========================
# Extrayendo datos
# ==========================
library("dplyr")
df=data.frame(table(enpClases[,1:2]))
names(df)=c("Unidad","Valor","frecuencia")

# Ordenar datos según un vector en un orden dado
# match da el resultado, por cada valor de datos$Clase en qué lugar está del vector clases
# y order los ordena.
df=df[order(match(df$Unidad,clases)),]
df$Valor=gsub("en ","\nen ",df$Valor)
df$Valor=gsub("de ","\nde ",df$Valor)
df$Valor=gsub(" C","\nC",df$Valor)
df$Valor=gsub(" dig","\ndig",df$Valor)

# Por niveles (ESO,BAC)
df$Nivel=substr(df$Unidad,3,5)
df$Curso=substr(df$Unidad,1,5)
df$Clase=substr(df$Unidad,7,7)

# Datos por niveles y por cursos
niveles=aggregate(cbind(frecuencia)
                  ~Nivel+Valor,data=df,FUN=sum,na.rm=TRUE)
cursos = aggregate(cbind(frecuencia)
                   ~Curso+Valor,data=df,FUN=sum,na.rm=TRUE)

#A1) Ordenar por vector datos
# clases=unique(datos$Clase)
curs=c("1 ESO","2 ESO","3 ESO","4 ESO","1 BAC","2 BAC","1 CIC","2 CIC")
cursos=cursos[order(match(cursos$Curso,curs)),]  

# convierto el data frame a tabla, con la función xtabs
tcursos = xtabs(frecuencia ~ Curso+Valor, data=cursos)

#A2) Frecuencias relativas
cursos=data.frame(prop.table(tcursos,margin=1)) # acumulado en nivel
names(cursos)=c("Curso","Valor","frecuencia")
cursos$frecuencia=round(cursos$frecuencia*100,1)
curs=c("1 ESO","2 ESO","3 ESO","4 ESO","1 BAC","2 BAC","1 CFGS","2 CFGS")
cursos=cursos[order(match(cursos$Curso,curs)),]  

cursos2=data.frame(prop.table(tcursos,margin=2)) # acumulado en nivel
names(cursos2)=c("Curso","Valor","frecuencia")
cursos2$frecuencia=round(cursos2$frecuencia*100,1)
cursos2=cursos2[order(match(cursos2$Curso,curs)),] 

# ===========================
# A) Plataformas digitales
# ===========================
colores=colorRampPalette(brewer.pal(9, "OrRd"))(35)

ggplot(cursos,aes(x=reorder(Curso,order(match(Curso,curs))), y=Valor)) +
  scale_color_manual(values = colores, guide=FALSE) +
  geom_point(data=cursos,aes(color=as.factor(frecuencia)
                 , size=frecuencia), alpha=0.9)+
  geom_point(data=cursos,aes(size=frecuencia),shape=21,colour="black",stroke=1.5)+ #añade bordes negros
  scale_size(range = c(0, 16), name="Frecuencia") +
  guides(size=guide_legend(override.aes=list(colour=colores[seq(1,31,length.out=4)]))) + # cambia los colores de la leyenda
  xlab("Cursos")+ylab("Número de respuestas") + # etiquetas X e Y
  labs(title="Uso de distintas plataformas digitales - Cursos(%)")+ # título
  theme(plot.title = element_text(lineheight=.8, # título
                     face="bold", hjust="0.5",size=24,color="#4E0363"                       ,family="comic",margin=margin(t=10,b=10))) +
  # # scale_x_discrete(labels=abbreviate)+ # abreviar datos en eje X
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
  theme(legend.position = "right",legend.spacing.x = unit(0.5,"cm"))+
  # posición de la leyenda y espaciado
  theme(legend.title=element_blank())+ # Quito el nombre de la leyenda
  theme(legend.text = element_text(size=8,colour="#241484",face="bold"))+ #tipo de texto
  theme(legend.background =
          element_rect(fill="#E8F5FB",colour="#4E0363",size=2))+ # caja de la leyenda
  theme(legend.margin =margin(r=10,l=5,t=5,b=5), legend.box.margin = margin(l=20,r=20)) +
  theme(legend.box.just = "top")+
  # guides(fill=guide_legend(nrow=1)) + # Distribuyo la leyenda en una fila
  theme(axis.text.x=
          element_text(colour="#241484",size=10,face="bold",angle=90,vjust=0.5))+ # tamaño letra eje X y vjust para centrar
  theme(axis.text.y=
          element_text(colour="#4E0363",size=11,face="bold",hjust=0.5)) + # tamaño letra eje Y
  theme(panel.grid.minor = element_blank()) +
  # annotate("text",x=cursos$Curso,y=cursos$Valor
  #          ,label=paste0(cursos$frecuencia,"%")
  #          ,hjust=-0.2,vjust=-1.5
  #          ,cex=4.5,col="#241484",fontface="bold",family="comic" #fontface="bold"
  # )
  annotate("text",x=cursos2$Curso,y=cursos2$Valor
           ,label=paste0(cursos2$frecuencia,"%")
           ,hjust=+1.6,vjust=+1.9 #4E0363
           ,cex=4,col="#B31B71",fontface="bold",family="comic" #fontface="bold"
  )




# ==================================
# B) Plataformas digitales (niveles)
# ==================================

# =================================
# C) Plataformas digitales (cursos)
# =================================
