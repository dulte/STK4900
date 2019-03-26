wcgs=read.table("http://www.uio.no/studier/emner/matnat/math/STK4900/data/wcgs.txt",sep="\t", header=T,na.strings=".")

#a)
wcgs$behcat=factor(wcgs$behpat)
wcgs.beh=glm(chd69~age_10+chol_50+sbp_50+bmi_10+smoke+behcat, data=wcgs, family=binomial, subset=(chol<600))
summary(wcgs.beh)

#Only cat. 2 seems to be significant

#b)
wcgs.beh2=glm(chd69~age_10+chol_50+sbp_50+bmi_10+smoke+dibpat, data=wcgs, family=binomial, subset=(chol<600))
anova(wcgs.beh2,wcgs.beh, test="Chisq")

#This gives a very small G statistics, thus a p value of 0.9, meaning that there is no significant difference between the groups!

#c)
wcgs.resc=glm(chd69~age_10+chol_50+bmi_10+sbp_50+smoke, data=wcgs, family=binomial, subset=(chol<600))
anova(wcgs.resc,wcgs.beh2,wcgs.beh, test="Chisq")


#There seem that there is a significant difference if one choose a behaviour. But not which of A/B1 or A/B2

#d)
expcoef=function(glmobj)
{
  regtab=summary(glmobj)$coef
  expcoef=exp(regtab[,1])
  lower=expcoef*exp(-1.96*regtab[,2])
  upper=expcoef*exp(1.96*regtab[,2])
  cbind(expcoef,lower,upper)
}

expcoef(wcgs.resc)
expcoef(wcgs.beh)
expcoef(wcgs.beh2)


#One should choose B1/2. If one instead choose A1/2 the odds of getting CHD increases 2 fold