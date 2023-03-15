# 1) Datos
l=c(100,	110,	120,	130,	140,	150,	160,	170,	180,	190,	200,	210,	220,	230,	240,	250,	260)
T=c(2.01,	2.11,	2.20,	2.29,	2.37,	2.46,	2.5,	2.62,	2.69,	2.79,	2.84,	2.91,	3.2,	3.04,	3.11,	3.17,	3.14)
l = l /100 # porque está en centímetros y hay que pasar a metros

# 2) transformar 
raizl = l ** 0.5
plot(raizl,T)
l1=lm(T~raizl)
abline(l1)

m = l1$coefficients[2]
g = (4 * pi ** 2) / (m ** 2)
g

summary(l1)$r.squared