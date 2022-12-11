# =======================
# librerías y condiciones
# =======================
cat("\014") 

library(ggplot2)
library(RColorBrewer) # gama de colores
library(showtext) #Fuentes para ggplot
library(scales)   # Need the scales package
showtext_auto() # Para usar y cargar fuentes para ggplot
# familias disponibles en Google
# fuentes = font_families_google(db_cache = TRUE, handle = curl::new_handle())
font_add_google("Just Another Hand","amano")
font_add("yrsa","/usr/share/fonts/truetype/fonts-yrsa-rasa/Yrsa-Regular.ttf")
font_add("comic","comic.ttf")

# ===============
# datos iniciales
# ===============
datos = read.csv2("~/R/propuesta_mejora_Moodle/probando_cs_preguntas.csv",header=TRUE,sep=";",
                  stringsAsFactors = FALSE)

# =================
# otros data frames
# =================

out = split(datos,f=datos$Pregunta)  #obtiene una lista con diferentes dataframes

# marca temporal, nombre y apellidos, departamento
mt=out$`Marca temporal`[,4] #vector de la columna 4
nombre = out$`Nombre y apellidos`[,4] #vector de la columna 4
dpto = out$`Departamento`[,4] #vector de la columna 4

# Si tengo formación en Séneca, si uso Cuaderno Séneca, formación en Moodle
fSeneca=out$`Sobre la formación en Séneca`[,c(3:5)] #dataframe 3 y 4 y 5
uCuaderno=out$`¿Estás usando actualmente el cuaderno de Séneca?`[,c(4:5)] #df 4 y 5
fMoodle=out$`Participación en la formación sobre Moodle Centros`[,c(4:5)] #df 4 y 5

# Enseñanza no presencial: clases y número alumnos
enpClases=out$`Enseñanza no presencial.`[,c(3:5)] #dataframe 3 y 4 y 5
enpNumero=out$`¿Cuántos alumnos usan en tus clases cada herramienta digital aproximadamente?`[,c(3:5)] #dataframe 3 y 4 y 5

# Remove out
rm(out)



             
             

