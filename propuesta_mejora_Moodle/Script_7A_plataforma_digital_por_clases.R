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

#A1) Ordenar por vector datos
# clases=unique(datos$Clase)
clases=c("1 ESO A","1 ESO B","1 ESO C","1 ESO D","1 ESO E",
         "2 ESO A","2 ESO B","2 ESO C","2 ESO D","2 ESO E",
         "3 ESO A","3 ESO B","3 ESO C","3 ESO D","3 ESO E",
         "4 ESO A","4 ESO B","4 ESO C","4 ESO D","4 ESO E",
         "1 BACH A","1 BACH B","1 BACH C","1 BACH D",
         "2 BACH A","2 BACH B","2 BACH C","2 BACH D","1 CIC A","2 CIC A")
# Ordenar datos según un vector en un orden dado
# match da el resultado, por cada valor de datos$Clase en qué lugar está del vector clases
# y order los ordena.
df=df[order(match(df$Unidad,clases)),]

# Por niveles (ESO,BAC)
df$Nivel=substr(df$Unidad,3,5)
df$Curso=substr(df$Unidad,1,5)
df$Clase=substr(df$Unidad,7,7)

# Datos por niveles y por cursos
niveles=aggregate(cbind(frecuencia)
                  ~Nivel+Valor,data=df,FUN=sum,na.rm=TRUE)
cursos = aggregate(cbind(frecuencia)
                   ~Curso+Valor,data=df,FUN=sum,na.rm=TRUE)

df

# ===========================
# A) Plataformas digitales
# ===========================
ggplot(df,aes(x=reorder(Unidad,order(match(Unidad,clases))), y=frecuencia, fill=Valor)) + # datos y aestética
  geom_bar(width = 0.75, stat='identity', colour="black", position="dodge") +  # geometría básica de barra
  scale_fill_brewer(palette="Set1", name="Plataformas digitales") +
  # o bien directamente
  # scale_fill_manual(values = rev(brewer.pal(5,"Set1"))
  #                  , name="Uso del cuaderno de Séneca") +
  scale_y_continuous(breaks=seq(0,max(df$frecuencia),1)) +
  # scale_fill_hue(c=100,l=50) + #valores de cromaticidad y luminancia
  xlab("Unidades")+ylab("Número de respuestas") + # etiquetas X e Y
  labs(title="Uso de distintas plataformas digitales")+ # título
  theme(plot.title = element_text(lineheight=.8, # título
                    face="bold", hjust="0.5",size=24,color="#4E0363",family="comic",
                    margin=margin(t=10,b=10)))+
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
  theme(axis.ticks.x=element_blank())+ # Quito las marcasdel eje X
  theme(legend.position = "top",legend.spacing.x = unit(0.5,"cm"))+
  # posición de la leyenda y espaciado
  theme(legend.title=element_blank())+ # Quito el nombre de la leyenda
  theme(legend.text = element_text(size=8,colour="#241484",face="bold"))+ #tipo de texto
  theme(legend.background =
          element_rect(fill="#E8F5FB",colour="#4E0363",size=2))+ # caja de la leyenda
  theme(legend.margin =margin(r=10,l=5,t=5,b=5)) +
  guides(fill=guide_legend(nrow=1)) + # Distribuyo la leyenda en una fila
  theme(axis.text.x=
          element_text(colour="#241484",size=10,face="bold",angle=90))+ # tamaño letra eje X
  theme(axis.text.y=
          element_text(colour="#241484",size=15,face="bold")) + # tamaño letra eje Y
  theme(panel.grid.minor = element_blank(),panel.grid.major.x = element_blank())

# ==================================
# B) Plataformas digitales (niveles)
# ==================================

# =================================
# C) Plataformas digitales (cursos)
# =================================
