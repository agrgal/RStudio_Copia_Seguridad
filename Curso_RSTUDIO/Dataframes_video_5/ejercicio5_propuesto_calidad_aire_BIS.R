calidadAire = airquality
calidadAire2 = calidadAire [ , c(3,4,5,6) ]
dim(calidadAire2)
calidadAire2$Date=paste0(calidadAire2$Day,
                         ".",calidadAire2$Month)
l1=lm(Temp~Wind,data=calidadAire2)
plot(x=calidadAire2$Wind,y=calidadAire2$Temp,xlim=c(0,30),ylim=c(0,100))
plot(calidadAire2$Wind,calidadAire2$Temp,xlim=c(0,30),ylim=c(0,100))
abline(l1)
m = l1$coefficients[2]
n= l1$coefficients[1]
summary(l1)$r.squared
diascalurosos= calidadAire2[calidadAire2$Temp>=90,]