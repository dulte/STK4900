melanom=read.table("http://www.uio.no/studier/emner/matnat/math/STK4900/data/melanoma.txt",header=T)

library(survival)

#a)
fit.sex=survfit(Surv(lifetime,status==1)~sex, data=melanom)
plot(fit.sex, lty=1:2, mark.time=F)
survdiff(Surv(lifetime,status==1)~sex, data=melanom)

#This indicates that there is a significant difference in the hazard across the sexes.

#b)

fit.thickn=survfit(Surv(lifetime,status==1)~factor(grthick), data=melanom)
plot(fit.thickn, lty=1:2, mark.time=F)
survdiff(Surv(lifetime,status==1)~factor(grthick), data=melanom)

#There also seems to ce a significance the thickness

#c)
#Same as above

#d)
coxfit.sex=coxph(Surv(lifetime,status==1)~factor(sex),data=melanom)
summary(coxfit.sex)

#You have twice the hazard as male

#e)
#Same as above...