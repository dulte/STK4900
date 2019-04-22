crabs = read.table("https://www.uio.no/studier/emner/matnat/math/STK4900/v19/mandatory/crabs.txt",header=TRUE)
plot(crabs)

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

crabs.fit.weight = glm(y~weight,data=crabs,family = binomial)
crabs.fit.color = glm(y~factor(color),data=crabs,family = binomial)
crabs.fit.spine = glm(y~factor(spine),data=crabs,family = binomial)

summary(crabs.fit.weight)
summary(crabs.fit.color)
summary(crabs.fit.spine)

#d)
crabs.fit.all <- glm(y~width + weight + factor(color) + factor(spine),data=crabs,family = binomial)
summary(crabs.fit.all)

crabs.fit.width.weight = glm(y~width + weight,data=crabs,family = binomial)
summary(crabs.fit.width.weight)

plot(width~weight,data=crabs, main="Width as a function of weight")

#e)

crabs.fit.width.weight.interaction = glm(y~width + weight + width:weight,data=crabs,family = binomial)
anova(crabs.fit.width.weight.interaction)
summary(crabs.fit.width.weight.interaction)

# There is confounding, but does not seem to be interaction
