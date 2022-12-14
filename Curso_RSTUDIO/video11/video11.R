# Video 11
# ========

notas=c("IN","SF","BI","NO","SB",
        "BI","NO","BI","NO","IN",
        "IN","SF","BI","IN","SF",
        "BI","IN","SF","BI","IN",
        "SF","BI","SB","IN","SF",
        "BI","NO","BI","NO","IN")

notas=as.factor(notas)
levels(notas)

notas=ordered(notas,levels=c("IN","SF","BI","NO","SB"))

table(notas)

cumsum(table(notas))

prop.table(table(notas))

cumsum(prop.table(table(notas)))

repetidor=c("N","N","N","R","R","N","N","R","R","R",
            "N","N","R","R","N","N","R","R","N","N",
            "N","N","R","R","N","N","R","R","N","N")

clase = data.frame(notas, repetidor)
t1=t(table(clase))
t1

t(apply(t1,MARGIN=1,FUN=cumsum))
t2 = t(apply(prop.table(t1)*100,MARGIN=1,FUN=cumsum))

barplot(t1,beside=TRUE,col=c("red","blue"))