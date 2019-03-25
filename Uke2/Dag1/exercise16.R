#a)
n1 = 935
n2 = 927
y1 = 230
y2 = 208

p1 = y1/n1
p2 = y2/n2

interval = p1 - p2

se1 = sqrt(p1*(1-p1)/n1)
se2 = sqrt(p2*(1-p2)/n2)

se = sqrt(se1**2 + se2**2)
lower = interval - se*1.96
upper = interval + se*1.96

margin = 1.96*se

cbind(interval,margin,lower,upper)

#The 95% confindant interval does contain 0, so we cannot conclude that there is a difference.

#b)
p=(y1+y2)/(n1+n2)
se0=sqrt(p*(1-p)/n1+p*(1-p)/n2)
z=(p1-p2)/se0
pval=2*(1-pnorm(abs(z)))
cbind(z,pval)

#We only have a p-value of 0.27, so we can not reject the null hypothesis. This is in line with the confidence interval

#c)
hoyre=matrix(c(y1,y2,n1-y1,n2-y2),nrow=2) 
prop.test(hoyre,correct=F)
# With the chi squared test we got the same restult. 
# We have chi**2 = x**2

#d)
# The same but for SP
n1 = 935
n2 = 927
y1 = 80
y2 = 101

p1 = y1/n1
p2 = y2/n2

interval = p1 - p2

se1 = sqrt(p1*(1-p1)/n1)
se2 = sqrt(p2*(1-p2)/n2)

se = sqrt(se1**2 + se2**2)
lower = interval - se*1.96
upper = interval + se*1.96

margin = 1.96*se

cbind(interval,margin,lower,upper)

#Zeros is again in the the confidance interval, so we can not conclude that there was a change.

p=(y1+y2)/(n1+n2)
se0=sqrt(p*(1-p)/n1+p*(1-p)/n2)
z=(p1-p2)/se0
pval=2*(1-pnorm(abs(z)))
cbind(z,pval)

#With a p-value if 0.09, we can not reject H0
