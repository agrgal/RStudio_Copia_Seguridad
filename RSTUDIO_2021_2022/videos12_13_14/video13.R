# video 13
pacientes = read.csv2("pacientes.csv",header=TRUE,sep=",")
edad = pacientes$Edad

#moda
cat("\014")
table(edad)
max(table(edad))s
which(table(edad)==max(table(edad)))
moda = names(which(table(edad)==max(table(edad))))
moda

#media
media = mean(edad)
media
sum(edad)/length(edad)
# o a través de la tabla
sum(table(edad)*as.numeric(names(table(edad))))/length(edad)