gehan=read.table("http://www.uio.no/studier/emner/matnat/math/STK4900/data/gehan.txt", header=T)
gehan
library(survival)
#a)
fit.1=survfit(Surv(time,cens)~treat, conf.type="none", data=gehan)
summary(fit.1)
plot(fit.1,lty=1:2)

print(fit.1)

#The median survival is ~8 weeks for the control, and ~23 weeks for the treatment

#b)

fit.2=survfit(Surv(time,cens)~treat, conf.type="plain",data=gehan)
summary(fit.2)
plot(fit.2, conf.int=T, lty=1:2)

#For the most part, there is no overlap of the CI's, so the treatments are most likly different

#c)
survdiff(Surv(time,cens)~treat, data=gehan)

#The Log-rank test gives a p value of almost 0, so the difference is significant

#d)
fit.4=coxph(Surv(time,cens)~factor(treat), data=gehan)
summary(fit.4)

#We see that someone in the treatment group has only 20% of the hazard compared to the control