ruleta = read.csv2("ruleta.csv",header=TRUE,sep=",")

# valores de media, mediana, varianza muestrales y desviación típica muestrales
medias = sapply(ruleta,mean)
medianas = sapply(ruleta,median)
varianzas = sapply(ruleta,var)
desviaciones = sapply(ruleta,sd)

# valores de varianza y desviación típica normales
# primero funciones
varN = function(x) { var(x)*(length(x)-1)/length(x)}
sdN = function(x) {sqrt(varN(x))}
# segundo calculos
varianzasNormales = sapply(ruleta,varN)
desviacionesNormales = sapply(ruleta,sdN)

# Matriz tipificada
ruletaTipificada = scale(ruleta,center=TRUE,scale=desviacionesNormales)
ruletaTipificada = data.frame(ruletaTipificada)
ruletaTipificada

# Con faltas y suspensos
faltas = c(5,7,3,4,6,7,2,5,5,2,7)
suspensos = faltas-2
suspensos[2]=1
cov(faltas, suspensos)
cov(faltas, suspensos)*(length(faltas)-1)/length(faltas)
# correlacion
cor(faltas, suspensos)
cov(faltas,suspensos)/(sd(faltas)*sd(suspensos)) # es la misma


# volvemos a la ruleta. Calculamos la covarianza. Su diagonal son las varianzas.
n=dim(ruleta)[1]
cov(ruleta)*(n-1)/n
varianzasNormales

# Calculamos la correlación de la ruleta
round(cor(ruleta),2)
# que es lo mismo que la covarianza de la matriz tipificada
n=dim(ruletaTipificada)[1]
round(cov(ruletaTipificada)*(n-1)/n,2)




