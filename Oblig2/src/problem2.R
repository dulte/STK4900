olympic=read.table("http://www.uio.no/studier/emner/matnat/math/STK4900/v17/olympic.txt",sep="\t",header=TRUE)

#a)


#b)
fit.null = glm(Total2000~offset(Log.athletes),data=olympic,family=poisson)
fit.pop = glm(Total2000~offset(Log.athletes)+Log.population,data=olympic,family=poisson)
fit.pop.gdp = glm(Total2000~offset(Log.athletes)+Log.population + GDP.per.cap,data=olympic,family=poisson)
fit.pop.gdp.96 = glm(Total2000~offset(Log.athletes)+Log.population + GDP.per.cap + Total1996,data=olympic,family=poisson)

fit.pop.96 = glm(Total2000~offset(Log.athletes)+GDP.per.cap + Total1996,data=olympic,family=poisson)

summary(fit.pop.96)


summary(fit.pop)
summary(fit.pop.gdp)
summary(fit.pop.gdp.96)
anova(fit.null,fit.pop,fit.pop.gdp,fit.pop.gdp.96,test="Chisq")


#Her virker det som Log.population of GDP.per.cap har forskjellig sigificant med of uten Total1996. Dette må diskuteres mer
# foer jeg svarer på oppgaven.