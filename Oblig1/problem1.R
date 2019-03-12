no2data <- read.table("http://www.uio.no/studier/emner/matnat/math/STK4900/v19/mandatory/no2.txt",sep="\t",header=TRUE)

#a)
summary(no2data$log.no2)
summary(no2data$log.cars)

boxplot(no2data$log.no2, ylab="log.no2",col="green")
title("Distribution of log.no2")
boxplot(no2data$log.cars, ylab="log.cars",col="red")
title("Distribution of log.cars")

plot(log.no2~log.cars,data=no2data)
title("log.no2 vs log.cars")

#There seems to be a linear dependence of log.no2 on log.cars.
#We can therefore go on try to fit a linear model


#b)
fit = lm(log.no2~log.cars,data=no2data)
summary(fit)

plot(log.no2~log.cars,data=no2data,col="grey")
abline(fit)
title("Linear Regression of log.no2 and log.cars")


#c)

#CPR plot to check linearity
library(car)
crPlots(fit,terms=~log.cars)
title("CPR plot of log.cars")

#Looks more or less linear, with some diviation for lower values

#Check of Homoscedasticity
standardres = rstandard(fit)
fit.standardres = lm(sqrt(abs(standardres))~fit$fit)
plot(fit$fit,sqrt(abs(standardres)),xlab="Fitted Values",ylab="sqrt(|Standard Residuals|)")
abline(fit.standardres)
title("Homoscedasticity Plot with Standardized Residuals")

#Sees that the variance is decreasing!

#Check for normality
hist(fit$res,breaks=(1+3.322*log(length(fit$res))),col="blue")
boxplot(fit$res,col="grey",ylab="Residual")
title("Distribution of the Residuals")
qqnorm(fit$res);qqline(fit$res)

#We see that the residuals are not normal

#d)
plot(no2data)


cv.R2=function(lmobj,y=lmobj$y,x=lmobj$x)
{
  a=t(x)%*%x
  d=diag(1/eigen(a)$values)
  e=eigen(a)$vector
  inv.a=e %*% d %*% t(e)
  v=diag(x%*%inv.a%*%t(x))
  SSkryss=sum((lmobj$res/(1-v))^2)
  SStot=sum((y-mean(y))^2)
  R2kryss=1-SSkryss/SStot
  R2kryss
}





fit.multi = lm(log.no2~log.cars+temp+wind.speed+hour.of.day,data=no2data, x=T, y=T)
summary(fit.multi)
anova(fit.multi)

r2 = cv.R2(fit.multi)

#TODO: Do forward selection with cv.R2

#e) 
