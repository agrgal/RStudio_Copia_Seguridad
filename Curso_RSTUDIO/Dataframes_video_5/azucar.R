# principio azúcar
productos=read.delim("openfood_sugar.csv",header=TRUE)
datos=productos[,c("product_name","sugars_100g")]
datos$sugars_100g[is.na(datos$sugars_100g)]=0

verdad=datos[datos$sugars_100g<=10,]
dim(verdad)
engañosos=datos[datos$sugars_100g>10 & datos$sugars_100g<=50,]
dim(engañosos)
mentira=datos[datos$sugars_100g>50,]
dim(mentira)