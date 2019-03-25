wcgs=read.table("http://www.uio.no/studier/emner/matnat/math/STK4900/data/wcgs.txt", sep="\t",header=T,na.strings=".")

#a)
table(wcgs$smoke,wcgs$chd69)

p1=159/1502
p0=98/1652
RR=p1/p0
OR=(p1/(1-p1))/(p0/(1-p0))
cbind(p1,p0,RR,OR)

#b)
fit.smoke=glm(chd69~smoke,data=wcgs,family=binomial)
print(fit.smoke)


#c)
fit.age=glm(chd69~age,data=wcgs,family=binomial)
print(fit.age)