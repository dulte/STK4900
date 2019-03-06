hers=read.table("http://www.uio.no/studier/emner/matnat/math/STK4900/data/hers.txt",sep="\t",header=T,na.strings=".")
hers.no=hers[hers$diabetes==0, ]

#a)
summary(hers.no$glucose[hers.no$exercise==0])
summary(hers.no$glucose[hers.no$exercise==1])
boxplot(hers.no$glucose~hers.no$exercise)
#It is difficult to see a clear differance in the median, but 
#there is some difference
#When the women exercise, the spread is decreased.


#b)
t.test(glucose~exercise, var.equal=T,data=hers.no)
#From the t test we see that the p value is 0.000113, which means that 
#there is a difference in mean in the two groups
#The CI does not contain 0, so again we see that there is a difference in the two means

#c)
fit.c=lm(glucose~exercise,data=hers.no)
summary(fit.c)
#This gives us that the mean of non-exercisers are 97.36 and 
#the mean for exercises 97.36-1.69. This difference is given 
#a p value of 0.000113, which is the same as in b)
#We also see that we get a p-value from the F-test. Once again it is 0.000113
#Since we only have one independent variable, we expect the p values from the F and the t test to be the same

#d)
fit.d=lm(glucose~exercise+age+BMI,data=hers.no)
summary(fit.d)

#We now get a better p value from the F test
#We can also see that exercise is now less significant (but still quite).
#The p value for age is too high for us to say that it is significant
#BMI seems to be the main predictor of glucose level, with a p value close to 0