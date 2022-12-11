# ===============
# datos Gsuite
# ===============

# A) Obtengo datos de diferencia con el día elegido
datos = read.csv("correos.csv",header=TRUE,sep=",")
datos$Retraso=as.POSIXct(datos$Fecha_hora,format="%Y/%m/%d %H:%M:%S",tz=Sys.timezone())
diaReferencia=as.POSIXct("2020/11/24 00:00:00",format="%Y/%m/%d %H:%M:%S",tz=Sys.timezone())
datos$Retraso=round(difftime(diaReferencia,datos$Retraso,units="days",0))
datos$Retraso=as.integer(datos$Retraso)
# https://stackoverflow.com/questions/25960517/how-to-convert-date-and-time-from-character-to-datetime-type

#A1) Ordenar por vector datos
# clases=unique(datos$Clase)
clases=c("1 ESO A","1 ESO B","1 ESO C","1 ESO D","1 ESO E",
         "2 ESO A","2 ESO B","2 ESO C","2 ESO D","2 ESO E",
         "3 ESO A","3 ESO B","3 ESO C","3 ESO D","3 ESO E",
         "4 ESO A","4 ESO B","4 ESO C","4 ESO D","4 ESO E","4 ESO X",
         "1 BACH A","1 BACH B","1 BACH C","1 BACH D",
         "2 BACH A","2 BACH B","2 BACH C","2 BACH D")
# Ordenar datos según un vector en un orden dado
# match da el resultado, por cada valor de datos$Clase en qué lugar está del vector clases
# y order los ordena.
datos=datos[order(match(datos$Clase,clases)),]

# B) Distintos subdataframes
nunca=datos[is.na(datos$Retraso),]
# Incluyo un vector Imprime
nunca$Imprime="N"
#Cambia algunos a sí

datos2=datos[complete.cases(datos),]
masMes=datos2[datos2$Retraso>=30,]
masDosSemanas=datos2[which(datos2$Retraso>=14 & datos2$Retraso<30),]
normal=datos2[datos2$Retraso<14,]
rm(datos2) #borrado de la matriz de complete cases

# C) Datos agregados por niveles
cursos=c("1 ESO","2 ESO","3 ESO","4 ESO","1 BAC","2 BAC")
datos$Nivel=substr(datos$Clase,3,5)
datos$Curso=substr(datos$Clase,1,5)
porClase=aggregate(Retraso~Clase,data=datos,FUN=median,na.rm=TRUE)
porClase=porClase[order(match(porClase$Clase,clases)),]
porCurso=aggregate(Retraso~Curso,data=datos,FUN=median,na.rm=TRUE)
porCurso=porCurso[order(match(porCurso$Curso,cursos)),]
# porCurso=porCurso[order(match(porCurso$Curso,clases)),]
porNiveles=aggregate(Retraso~Nivel,data=datos,FUN=median,na.rm=TRUE)
