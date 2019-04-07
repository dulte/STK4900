crabs = read.table("https://www.uio.no/studier/emner/matnat/math/STK4900/v19/mandatory/crabs.txt",header=TRUE)

#a)

#b)
crabs.fit.width = glm(y~width,data=crabs,family = binomial)

# Function used for the oddsratio, as given by the lecturer
expcoef=function(glmobj)
{
  regtab=summary(glmobj)$coef
  expcoef=exp(regtab[,1])
  lower=expcoef*exp(-1.96*regtab[,2])
  upper=expcoef*exp(1.96*regtab[,2])
  cbind(expcoef,lower,upper)
}
summary(crabs.fit.width)
expcoef(crabs.fit.width)

#c)


