# funciones
varN = function(x) { var(x)*(length(x)-1)/length(x)}
sdN = function(x) {sqrt(varN(x))}

# Obtenemos los datos
trigo = read.csv2("trigo_dos_variables.csv",header=TRUE,sep=",")

# (2) Medias, medianas
medias = sapply(trigo,mean)
medianas = sapply(trigo,median)

# (3) Varianzas y desviaciones típicas, muestrales y normales
varianzasM = sapply(trigo,var)
dTM = sapply(trigo,sd)
varianzasN = sapply(trigo,varN)
dTN = sapply(trigo,sdN) # cuando el número de casos es muy grande,
# las varianzas o desv típicas son parecidas

# (4) Matriz tipificada
# es restar la media y dividir entre la desviación típica normalizada
trigoTIP = scale(trigo,center=TRUE,scale=dTN)
trigoTIP = data.frame(trigoTIP)

# (5) Covarianza muestral y normal
n = dim(trigo)[1]
cov(trigo) # Muestral
cov(trigo)*(n-1)/n #Normalizada

# (6) Correlaciones
cor(trigo) # se puede decir que existe cierta relación entre la altura de las 
           # plantas y el número de granos.

# (7) Gráfica boxplot
boxplot(trigo,data=trigo,col=c("red","blue"))
boxplot(trigoTIP,data=trigoTIP,col=c("red","blue"))

# (8) Creo un factor con el número de granos, por ejemplo
vectorBreak =seq(min(trigo$granos)-1,max(trigo$granos)+1,5)
ngranos = cut (trigo$granos, breaks=vectorBreak, right=FALSE,
               labels=c("muy pocos","pocos","suficientes","bastantes","muchos"))
trigo$cuantos = ngranos # introduzco en el dataframe
# y represento por esos niveles
boxplot (altura~cuantos, data=trigo, col=terrain.colors(length(levels(ngranos))))