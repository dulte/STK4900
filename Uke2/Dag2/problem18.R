wcgs=read.table("http://www.uio.no/studier/emner/matnat/math/STK4900/data/wcgs.txt",sep="\t", header=T,na.strings=".")

#a)
fit.smoke=glm(chd69~smoke,data=wcgs,family=binomial)
summary(fit.smoke)

#With a p-value
