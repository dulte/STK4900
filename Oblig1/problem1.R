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









# Muliple regression with increasing Cross Validated R2
fit.multi = lm(log.no2~log.cars,data=no2data, x=T, y=T)
cv.R2(fit.multi)
fit.multi = lm(log.no2~log.cars + log(wind.speed),data=no2data, x=T, y=T)
cv.R2(fit.multi)
fit.multi = lm(log.no2~log.cars + log(wind.speed) + temp,data=no2data, x=T, y=T)
cv.R2(fit.multi)
fit.multi = lm(log.no2~log.cars + log(wind.speed) + temp + hour.of.day,data=no2data, x=T, y=T)
cv.R2(fit.multi)

summary(fit.multi)



#Plot of the CVR2 against the model added to the fit
models_added = c("log.cars","log wind.speed","temp","hour.of.day")
models_added = factor(models_added,levels=models_added)
r2_found = c(0.256,0.418,0.466,0.472)


plot(models_added,r2_found,ylab="Cross Validated R2",xlab="Variable Added to the Model")
title("Forward Selection for NO2")



#Model with all the predictor variables, with no log 
fit.multi_no_log = lm(log.no2~log.cars+temp+wind.speed+hour.of.day,data=no2data, x=T, y=T)
cv.R2(fit.multi_no_log)
summary(fit.multi_no_log)


#e) 

#Checks the model assumptions. Copy past of c)

#CPR plot to check linearity
library(car)
crPlots(fit.multi,terms=~log.cars + log(wind.speed) + temp + hour.of.day)


#Looks more or less linear, with some diviation for lower values

#Check of Homoscedasticity
standardres.multi = rstandard(fit.multi)
fit.multi.standardres = lm(sqrt(abs(standardres.multi))~fit.multi$fit)
plot(fit.multi$fit,sqrt(abs(standardres.multi)),xlab="Fitted Values",ylab="sqrt(|Standard Residuals|)")
abline(fit_multi.standardres)
title("Homoscedasticity Plot with Standardized Residuals of Full Model")

#Sees that the variance is decreasing!

#Check for normality
hist(fit.multi$res,breaks=(1+3.322*log(length(fit.multi$res))),col="blue")
boxplot(fit.multi$res,col="grey",ylab="Residual")
title("Distribution of the Residuals")
qqnorm(fit.multi$res);qqline(fit.multi$res)
