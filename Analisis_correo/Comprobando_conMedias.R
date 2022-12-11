# ===============
# datos iniciales
# ===============
cat("\014") 

library(printr)
# A) Obtengo datos de diferencia con el día elegido
fechaInicial="2021/02/09 00:00:00" #Formato año / mes / día
fechaPpo="2020/09/15 00:00:00" # primer día de clase en septiembre
datos = read.csv2("correos3.csv",header=TRUE,sep=",")
datos$Retraso=as.POSIXct(datos$Global,format="%Y-%m-%d %H:%M:%S",tz=Sys.timezone())
diaReferencia=as.POSIXct(fechaInicial,format="%Y/%m/%d %H:%M:%S",tz=Sys.timezone())
datos$Retraso=round(difftime(diaReferencia,datos$Retraso,units="days",0))
datos$Retraso=as.integer(datos$Retraso)
# # https://stackoverflow.com/questions/25960517/how-to-convert-date-and-time-from-character-to-datetime-type

# # A1) Ordenar por vector datos
clases=unique(datos$Clase)
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
# 
# # B) Distintos subdataframes
diaPpo =as.POSIXct(fechaPpo,format="%Y/%m/%d %H:%M:%S",tz=Sys.timezone())
diasDesdeComienzo=round(as.integer(difftime(diaReferencia,diaPpo,units="days",0)))
datos$Retraso[which(datos$Retraso>diasDesdeComienzo)]=diasDesdeComienzo
nunca=datos[is.na(datos$Retraso) | datos$Retraso>=diasDesdeComienzo,]
# Incluyo un vector Imprime
nunca$Imprime="N"
#Cambia algunos a sí
# nunca$Imprime[c(1:7,9,12,20)]="S"
nunca$Imprime="S" #ahora mismo las imprime todas

datos2=datos[complete.cases(datos),]
masMes=datos2[datos2$Retraso>=30 & datos2$Retraso<diasDesdeComienzo,]
masDosSemanas=datos2[which(datos2$Retraso>=14 & datos2$Retraso<30),]
normal=datos2[datos2$Retraso<14,]
rm(datos2) #borrado de la matriz de complete cases
#
#
# # C) Datos agregados por niveles
cursos=c("1 ESO","2 ESO","3 ESO","4 ESO","1 BAC","2 BAC")
datos$Nivel=substr(datos$Clase,3,5)
datos$Curso=substr(datos$Clase,1,5)
porClase=aggregate(Retraso~Clase,data=datos,FUN=mean,na.rm=TRUE)
porClase=porClase[order(match(porClase$Clase,clases)),]
porCurso=aggregate(Retraso~Curso,data=datos,FUN=mean,na.rm=TRUE)
porCurso=porCurso[order(match(porCurso$Curso,cursos)),]
porNiveles=aggregate(Retraso~Nivel,data=datos,FUN=mean,na.rm=TRUE)

# D) retrasos en acceso a Correo
datos$RCorreos=as.POSIXct(datos$correoMax,format="%Y-%m-%d %H:%M:%S",tz=Sys.timezone())
datos$RCorreos=round(difftime(diaReferencia,datos$RCorreos,units="days",0))
datos$RCorreos=as.integer(datos$RCorreos)
datos$RCorreos[which(datos$RCorreos>diasDesdeComienzo)]=diasDesdeComienzo
datos$RCorreos[is.na(datos$RCorreos)]=diasDesdeComienzo
porClaseCorreos=aggregate(RCorreos~Clase,data=datos,FUN=mean,na.rm=TRUE)
porClaseCorreos=porClaseCorreos[order(match(porClaseCorreos$Clase,clases)),]
# E) retrasos en acceso a GClassroom
datos$RClassroom=as.POSIXct(datos$Classroom,format="%Y-%m-%d %H:%M:%S",tz=Sys.timezone())
datos$RClassroom=round(difftime(diaReferencia,datos$RClassroom,units="days",0))
datos$RClassroom=as.integer(datos$RClassroom)
datos$RClassroom[which(datos$RClassroom>diasDesdeComienzo)]=diasDesdeComienzo
datos$RClassroom[is.na(datos$RClassroom)]=diasDesdeComienzo
porClaseClassroom=aggregate(RClassroom~Clase,data=datos,FUN=mean,na.rm=TRUE)
porClaseClassroom=porClaseClassroom[order(match(porClaseClassroom$Clase,clases)),]
# F) retrasos en acceso a Drive
datos$RDrive=as.POSIXct(datos$Drive,format="%Y-%m-%d %H:%M:%S",tz=Sys.timezone())
datos$RDrive=round(difftime(diaReferencia,datos$RDrive,units="days",0))
datos$RDrive=as.integer(datos$RDrive)
datos$RDrive[which(datos$RDrive>diasDesdeComienzo)]=diasDesdeComienzo
datos$RDrive[is.na(datos$RDrive)]=diasDesdeComienzo
porClaseDrive=aggregate(RDrive~Clase,data=datos,FUN=mean,na.rm=TRUE)
porClaseDrive=porClaseDrive[order(match(porClaseDrive$Clase,clases)),]