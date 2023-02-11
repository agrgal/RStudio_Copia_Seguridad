dfpearson = read.table("http://aprender.uib.es/Rdir/pearson.txt",header=TRUE)

l1=lm(Hijos~Padres,data=dfpearson)
plot(dfpearson)
abline(l1)
summary(l1)$r.squared