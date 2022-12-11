# ===============
# datos iniciales
# ===============
cat("\014") 

datos = read.csv2("~/R/Analisis_correo/sondeoEmail.csv",header=TRUE,sep=",")
datos = datos[c(-31),] # elimino la fila del total

names(datos)=c("Unidad","N","Envíos","Móvil","Otros","Adjunto","Negrita","Cursiva","Colores","Emoticono")

# porcentaje de alumnado que envía el correo
datos$porEnviados=round(100*datos$Envíos/datos$N,1)

# Por niveles (ESO,BAC)
datos$Nivel=substr(datos$Unidad,3,5)
datos$Curso=substr(datos$Unidad,1,5)
datos$Clase=substr(datos$Unidad,7,7)

# Datos por niveles y por cursos
niveles=aggregate(cbind(N,Envíos,Móvil,Otros,Adjunto,Negrita,Cursiva,Emoticono)
                  ~Nivel,data=datos,FUN=sum,na.rm=TRUE)
cursos = aggregate(cbind(N,Envíos,Móvil,Otros,Adjunto,Negrita,Cursiva,Emoticono)
                   ~Curso,data=datos,FUN=sum,na.rm=TRUE)

# =========================================================================
# =========================================================================

# Gráfica barplot porcentaje de participación
porPar= barplot(datos$porEnviados,
        ylab="Porcentajes de participación",
        main="Porcentaje de participación por clases",
        col=c("blue"),
        names.arg= datos$Unidad,
        ylim=c(0,100),
        cex.names = 0.6, 
        las=2, # orientación vertical
        font.axis=2, # bold 
        axis.lty=1, # línea horizontal
        axes=TRUE)
text(porPar,cex=0.7,paste0(round(datos$porEnviados,1),"%")
     ,y=datos$porEnviados+5,col="black",srt=90,font=2)



# porcentaje de alumnado que envía el correo
niveles$porEnviados=round(100*niveles$Envíos/niveles$N,1)
cursos$porEnviados=round(100*cursos$Envíos/cursos$N,1)

# Gráfica barplot porcentaje de participación (NIVELES)
porPar= barplot(niveles$porEnviados,
                ylab="Porcentajes de participación",
                main="Porcentaje de participación por niveles",
                col=c("blue"),
                names.arg= niveles$Nivel,
                ylim=c(0,100),
                cex.names = 0.6, 
                las=2, # orientación vertical
                font.axis=2, # bold 
                axis.lty=1, # línea horizontal
                axes=TRUE,)
text(porPar,cex=0.7,paste0(round(niveles$porEnviados,1),"%")
     ,y=niveles$porEnviados+5,col="black",srt=90,font=2)

# Gráfica barplot porcentaje de participación (CURSOS)
porPar= barplot(cursos$porEnviados,
                ylab="Porcentajes de participación",
                main="Porcentaje de participación por cursos",
                col=c("blue"),
                names.arg= cursos$Curso,
                ylim=c(0,100),
                cex.names = 0.6, 
                las=2, # orientación vertical
                font.axis=2, # bold 
                axis.lty=1, # línea horizontal
                axes=TRUE,)
text(porPar,cex=0.7,paste0(round(cursos$porEnviados,1),"%")
     ,y=cursos$porEnviados+5,col="black",srt=90,font=2)
