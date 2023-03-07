  #Apartado 1
  trigo= read.csv("trigo - pareja5.csv", header= TRUE, sep = ",")
  #Apartado 2
  altura=trigo$altura
  media=mean(altura)
  media
  mediana=median(altura)
  mediana
  moda=which(table(altura)==max(table(altura)))
  moda = names(moda) #obtiene el valor verdadero de la moda 
  #Apartado 3
  ord=sort(altura)
  ord
  ord[c(900,1000,1001)]
  
  #Apartado 4
  quant25 = quantile(altura,0.25)
  quant75 = quantile(altura,0.75)
  perc35 = quantile(altura,0.35)
  #Apartado 5
  rango = diff(range(altura))
  rangoIC = quantile(altura, 0.75)-quantile(altura,0.25)
  #Apartado 6IQR()
  n=length(altura)
  #Varianza muestral
  varianzaMuestral = var(altura)
  #Desviación típica muestral
  DTmuestral = sd(altura)
  #Varianza normal (verdadera)
  varianzaNormal = var(altura)*(n-1)/n
  #Desviacion tipica normal
  DTnormal = sd(altura)*(sqrt((n-1)/n))
  #Apartado 7
  altura[c(2000)]=max(altura)*1.5
  tail(altura,5)
  #Apartado 8
  boxplot(altura,
          xlab="Trigo",ylab="Altura",notch = TRUE,col = c("blue"))
  abline(h=mean(altura),col=c("orange"))