wcgs=read.table("http://www.uio.no/studier/emner/matnat/math/STK4900/data/wcgs.txt",sep="\t", header=T,na.strings=".")

#a)
fit.smoke=glm(chd69~smoke,data=wcgs,family=binomial)
summary(fit.smoke)

#With a p-value of near 0, this is significant.

beta = fit.smoke$coefficients['smoke']
se = summary(fit.smoke)$coeff[,2]["smoke"]

margin = 1.96*se

lower = beta - margin
upper = beta + margin

cbind(beta,lower,upper)

OR = exp(beta)
OR_lower = exp(lower)
OR_upper = exp(upper)

cbind(OR,OR_lower,OR_upper)

#b)

expcoef=function(glmobj)
{
  regtab=summary(glmobj)$coef
  expcoef=exp(regtab[,1])
  lower=expcoef*exp(-1.96*regtab[,2])
  upper=expcoef*exp(1.96*regtab[,2])
  cbind(expcoef,lower,upper)
}

expcoef(fit.smoke)

#Same as in a)

#c)
fit.age=glm(chd69~age,data=wcgs,family=binomial)
summary(fit.age)

expcoef(fit.age)

#d)
fit.age10=glm(chd69~I(age/10),data=wcgs,family=binomial)
summary(fit.age10)

expcoef(fit.age10)
