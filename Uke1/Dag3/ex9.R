income = c(47.35,29.26,52.14,32.15,40.86,19.19,27.23,25.6,54.14,26.72,38.84,32.99,32.95,21.69,27.9,56.7,37.69,39.94)
risk_aversion = c(7,5,10,6,4,5,4,6,9,5,2,7,4,3,5,1,8,6)
insurance = c(140,45,180,160,90,10,35,35,190,35,75,70,55,10,40,175,95,95)

df = data.frame(income,risk_aversion,insurance)
plot(df)


fit.income_insurance = lm(insurance~income)
summary(fit.income_insurance)


fit.all = lm(insurance~income + risk_aversion)
summary(fit.all)

#There seems to be some curvature in the correlation between income and insurance
#We can therefore try

fit.poly = lm(insurance~risk_aversion + income + I(income^2))
summary(fit.poly)

#R2 is now bigger, but the P values for the variables are now larger.

#We can try with a sqrt
fit.sqrt = lm(insurance~risk_aversion + income + sqrt(income))
summary(fit.sqrt)

#Gives a bit lower R2 value