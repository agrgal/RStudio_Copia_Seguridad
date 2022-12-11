medioambiente=read.csv("environment-indicators-for-spain-1.csv",header=TRUE)

str(medioambiente)

co2=medioambiente[medioambiente$Indicator.Name=="CO2 emissions from solid fuel consumption (kt)",]$Value
str(co2)

catco2= cut(co2,breaks= seq((max(co2)+1),(min(co2)-1),length.out=5),labels = c("bajo" , "medio" , "alto", "muy alto"),right = FALSE)

catco2= ordered(catco2,levels= c("bajo" , "medio" , "alto", "muy alto"))

sort(catco2, decreasing = FALSE)

catco2ordenado= table(catco2)

barplot(catco2ordenado,ylim =c(0, 50),col =c("blue","yellow","red","orange"))

catco2ordenado2= prop.table(catco2ordenado)*(100)

barplot(catco2ordenado2,ylim =c(0, 100),col =c("blue","yellow","red","orange"))