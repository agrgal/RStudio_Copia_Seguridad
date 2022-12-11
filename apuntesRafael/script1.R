sal=c(1,2,3,4,5,6,7,7.5,8,8.5)
tiempo=c(22,23,23.5,24,24.7,25,25.1,25.3,26,28)
plot(sal,tiempo)
l1=lm(tiempo~sal)
abline(l)
l1
