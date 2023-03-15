# DATOS
años=c(100 ,	200 ,	300, 	400, 	500, 	600, 	700, 	800 ,	900 ,	1000 ,	1100 ,	1200, 	1300, 	1400, 	1500, 	1600, 	1700, 	1800, 	1900 ,	2000, 	2100 	,2200 ,	2300 	,2400 ,	2500)
N = c(1.952 ,	1.885,	1.878,	1.784,	1.851,	1.899,	1.691,	1.826,	1.739,	1.611,	1.616,	1.707,	1.52,	1.532,	1.543,	1.556,	1.583,	1.37,	1.5,	1.486,	1.477,	1.413,	1.396,	1.377,	1.248)
N= N*1e14 #El número de átomos de carbono 14

# fórmula es
# N = No exp (-t/tau)  luego ln(N) = ln(No) -t / tau --> m = -1/tau  y  n = ln(No)

x = años
y = log(N)

plot (x,y)
l1 = lm(y~x)
abline(l1)

# coeficientes
m = l1$coefficients[2]
n = l1$coefficients[1]
RR = summary(l1)$r.squared

# Valores
No = exp(n)
tau = -1 / m 
NoFormateado = formatC(No, format = "e", digits = 4)  
print(paste0("Número de átomos de carbono 14 al principio de la muestra ",NoFormateado))
print(paste0("La vida media de la muestra es: ",tau," años"))

# Cálculos
mat = 12.0107
Navo = 6.022e23 
masaCarbono = 20 * 20/100
print(paste0("Masa de carbono: ",masaCarbono," Kg"))
nmoles = masaCarbono*1000/mat
print(paste0("Nº moles de carbono: ",nmoles," moles"))
natomosdecarbono = nmoles*Navo
print(paste0("Nº moles de átomos de carbono: ",natomosdecarbono," nº átomos"))

# proporción de carbono 14 es 
proporcion = No / natomosdecarbono
print(paste0("proporción de átomos de C-14: ",proporcion*100," %"))



  