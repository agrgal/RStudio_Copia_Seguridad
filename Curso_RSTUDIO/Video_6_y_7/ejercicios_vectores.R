# Ejercicio sobre dataframes
cat("\014")
rm(list = ls()) # borrar el environment

# Obtener el vector "valores", de 1000 elementos, de valores la cadena repetida "2,2,3,4,1"
valores=rep(c(2,2,3,4,1),200)
# valores2=rep(c(2,2,3,4,1),length.out=1000)

# Obtener el vector "secuencia", con 1000 elementos, que empiece en 0 y acabe en 1, y con todos sus elementos igualmente espaciados.
secuencia=seq(0,1,length.out=1000)

# Obtener el vector "impares", de 500 elementos, con los primeros 500 números impares naturales.
impares=seq(1,by=2,length.out=500)
# Obtener el vector "pares", de 500 elementos, con los primeros 500 números pares naturales.
# pares=impares+1
pares = seq(2,by=2,length.out=500)

# Unir los vectores "impares" y "pares". Deberá tener 1000 elementos. llamar a la unión "numeros"
numeros = c(impares,pares)

# Obtener el vector "hiperbole", de 1000 elementos, resultado de aplicar la función coseno hiperbólico (búscala o constrúyela) a dividir entre 100 el vector "numeros"
hiperbole = cosh(numeros/100)

# Obtener un vector llamado "transformado" que será la concatenación de dos vectores:
# La suma del vector "impares" más "pares"
# La diferencia entre el vector "pares" e "impares"
transformado = c(impares+pares, pares-impares)

# Ordena el vector "transformado"
sort(transformado)

# Obtén la media del vector "transformado"
mean(transformado)

# Obtener un vector, llamado "cadatres", con los valores del vector transformado tomados de tres en tres.
# seq(3,1000,by=3)
cadatres=transformado[seq(3,1000,by=3)]
head(transformado,30)
head(cadatres,10)
