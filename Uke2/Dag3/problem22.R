cancerdata=hers=read.table("http://www.uio.no/studier/emner/matnat/math/STK4900/data/cancer.txt")
names(cancerdata)=c("age","cig","pyr","cancer")
cancerdata

#a)
cancerfit.1=glm(cancer~offset(log(pyr))+age+cig, data=cancerdata, family=poisson)
summary(cancerfit.1)

#Cig is significant

expcoef=function(glmobj)
{
  regtab=summary(glmobj)$coef
  expcoef=exp(regtab[,1])
  lower=expcoef*exp(-1.96*regtab[,2])
  upper=expcoef*exp(1.96*regtab[,2])
  cbind(expcoef,lower,upper)
}
expcoef(cancerfit.1)

#Rates increase by 6% if one smokes

#b)

cancerfit.3=glm(cancer~offset(log(pyr))+ age+I(age^2)+cig+I(cig^2), data=cancerdata, family=poisson)
summary(cancerfit.3)

anova(cancerfit.1,cancerfit.3)
