cirrhosis = read.table("https://www.uio.no/studier/emner/matnat/math/STK4900/v19/mandatory/cirrhosis.txt",header=TRUE)

#a)
library(survival)
surv_treat = survfit(Surv(time,status)~treat, data=cirrhosis, conf.type="none")
surv_sex = survfit(Surv(time,status)~sex, data=cirrhosis, conf.type="none")
surv_asc = survfit(Surv(time,status)~asc, data=cirrhosis, conf.type="none")
surv_agegr = survfit(Surv(time,status)~agegr, data=cirrhosis, conf.type="none")


plot(surv_treat,lty=1:2,xlab="Time [Days]",ylab="Survival",main="Survival for Different Treatment")
legend(5,0.2,c("prednisone","placebo"),lty=1:2)

plot(surv_sex,lty=1:2,xlab="Time [Days]",ylab="Survival",main="Survival for Different Sex")
legend(5,0.2,c("Female","Male"),lty=1:2)

plot(surv_asc,lty=1:3,xlab="Time [Days]",ylab="Survival",main="Survival for Different Ascites")
legend(5,0.2,c("None","Slight","Marked"),lty=1:3)

plot(surv_agegr,lty=1:3,xlab="Time [Days]",ylab="Survival",main="Survival for Different Ages")
legend(5,0.2,c("<50","50-65",">65"),lty=1:3)



#b)
survdiff(Surv(time,status)~treat, data=cirrhosis)
survdiff(Surv(time,status)~sex, data=cirrhosis)
survdiff(Surv(time,status)~asc, data=cirrhosis)
survdiff(Surv(time,status)~agegr, data=cirrhosis)

#c)

fit.all=coxph(Surv(time,status==1)~factor(sex)+factor(treat)+factor(asc)+age ,data=cirrhosis)
summary(fit.all)
