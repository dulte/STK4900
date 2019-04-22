olympic=read.table("http://www.uio.no/studier/emner/matnat/math/STK4900/v17/olympic.txt",sep="\t",header=TRUE)

plot(olympic)

#a)


#b)
fit.null = glm(Total2000~offset(Log.athletes),data=olympic,family=poisson)
fit.pop.gdp.96 = glm(Total2000~offset(Log.athletes)+Log.population + GDP.per.cap + Total1996 ,data=olympic,family=poisson)
fit.gdp.96 = glm(Total2000~offset(Log.athletes)+GDP.per.cap + Total1996,data=olympic,family=poisson)

summary(fit.pop.gdp.96)
summary(fit.gdp.96)



fit.pop = glm(Total2000~offset(Log.athletes)+Log.population,data=olympic,family=poisson)
fit.pop.gdp = glm(Total2000~offset(Log.athletes)+Log.population + GDP.per.cap,data=olympic,family=poisson)

summary(fit.pop)
summary(fit.pop.gdp)



