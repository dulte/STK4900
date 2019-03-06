hers.sample=read.table("http://www.uio.no/studier/emner/matnat/math/STK4900/v17/hers.sample.txt",header=T)

#a)
plot(hers.sample$age,hers.sample$sbp)
#It is difficult to see a clear linear relation between age and sbp

#b)
hers.fit.b=lm(sbp~age,data=hers.sample)
summary(hers.fit.b)
abline(hers.fit.b)
#seems that sbp increases by 0.44 per year
#When a person is born, this idicates that the sbp is 105.7
#But this is not very meaningful, since the study most likly didn't use babies.
#Both have good segnificance

#c
hers.fit.c=lm(sbp~I(age-67),data=hers.sample)
summary(hers.fit.c)
#The intercept is now highter. This is because we have age_0 = 67 
#Since the measurements includes women at age 67 we can interpret the intercept as the spb at age_0=67

#d
hers.fit.d=lm(sbp~I(age/10),data=hers.sample)
summary(hers.fit.d)
#Since the x values are 10x smaller, the slope has to be 10x greater to get the same result