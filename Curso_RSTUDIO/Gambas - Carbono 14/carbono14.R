#datos
annos=c(100,	200,	300,	400,	500,	600,	700,	800,	900,	1000,	1100,	1200,	1300,	1400,	1500,	1600,	1700,	1800,	1900,	2000,	2100,	2200,	2300,	2400,	2500)
N=c(1.952,	1.885,	1.878,	1.784,	1.851,	1.899,	1.691,	1.826,	1.739,	1.611,	1.616,	1.707,	1.52,	1.532,	1.543,	1.556,	1.583,	1.37,	1.5,	1.486,	1.477,	1.413,	1.396,	1.377,	1.248)

#Reorganizamos los datos
N = N* 1e14 # hay que elevarlo a exp 14

# cĺculo de los parámetros: N = No exp (-t/tau)
# log(N) = log(N0) - t/tau 
x = annos
y = log(N)

# recta de regresión
plot(x,y)
l1=lm(y~x)
abline(l1)

# parámetros
m = l1$coefficients[2]
n = l1$coefficients[1]
m
n

# primera pregunta
tau = -1/m
N0 = exp(n)
print(paste0("tau: ",tau," años"))
print(paste0("N0: ",N0))

#Segunda pregunta: fiabilidad
print(paste0("La fiabilidad es: ", summary(l1)$r.squared))

# Tercera
m = 5 #5 kg. 
NoAv = 6.022e23 # partículas en un mol.
matCarbono = 12.0107 # gr/mol
molesC = m*1000/matCarbono
print(paste0("Moles de Carbono en la muestra original: ",molesC))
NpartC = NoAv*molesC
print(paste0("Partículas de carbono en el árbol: ",NpartC))
xcientoCarbono14 = 100*N0/NpartC
print(paste0("%partículas de carbono 14: ",xcientoCarbono14))



