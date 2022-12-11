# =======================
# librerías y condiciones
# =======================
source("~/R/propuesta_mejora_Moodle/datos.R")

substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}

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

df$num=substrRight(as.character(df$Intervalos),3)
df[df$Intervalos=="Más de 100",]$num="110"
df[df$num=="e 5",]$num="5"
df[df$num=="uno",]$num="0"
df$num=as.numeric(df$num)

# convierto el data frame a tabla, con la función xtabs
tdf = xtabs(frecuencia ~ num+Valor, data=df)
# tdf

# df2=df[df$Valor=="Correos\ndigitales",c(3,4)]
df2=df[df$Valor=="Google\nClassroom",c(3,4)]
df2

ggplot(df2,aes(x=num))+geom_density(aes(color=frecuencia))

