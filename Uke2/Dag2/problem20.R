insects=read.table("http://www.uio.no/studier/emner/matnat/math/STK4900/data/insects.txt",header=T)

#a)

proportion<-insects$DEAD/insects$NUMBER
plot(insects$LOGDOSE, proportion, ylim=c(0,1),pch=16)

#b)
attach(insects)

fit<-glm(cbind(DEAD,NUMBER-DEAD)~LOGDOSE, data=insects,family=binomial)
summary(fit)

exp(1.0107)


#c)
pred.prop=predict(fit,type="response")
points(insects$LOGDOSE ,pred.prop)
logdose=seq(0.4,1,0.01)
new.doses=data.frame(LOGDOSE=logdose)
lines(logdose,predict(fit,new.doses,type="response"))


#d)

#LD50 is logdose ~0.7