V=c(13.51, 	11.00, 	8.70, 	8.44, 	7.50, 	6.30, 	6.14, 	5.63, 	5.19, 	4.82, 	4.50, 	4.22, 	4.00, 	3.60, 	3.55)
p=c(5 ,	6, 	7, 	8, 	9 ,	10, 	11, 	12, 	13, 	14, 	15, 	16, 	17, 	18, 	19)
InvV = 1 / V
plot(InvV,p)
l1=lm(p~InvV)
abline(l1)
a= l1$coefficients[2]
R = 0.082
T = 305
n = a/ (R*T)
n
summary(l1)$r.squared


