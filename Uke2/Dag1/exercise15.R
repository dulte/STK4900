#a)

n=935
y=309
p=y/n
se=sqrt(p*(1-p)/n)
margin=1.96*se
lower=p-margin
upper=p+margin
cbind(p,margin,lower,upper)

#This means that 33+- 3% will vote Ap. In other words, we have the interval (30%,36%)

#b)
#Frp
n=935
y=122
p=y/n
se=sqrt(p*(1-p)/n)
margin=1.96*se
lower=p-margin
upper=p+margin
cbind(p,margin,lower,upper)

#The interval for the amout who would have voted FrP is (10.8%,15.2%)

#Sp
n=935
y=80
p=y/n
se=sqrt(p*(1-p)/n)
margin=1.96*se
lower=p-margin
upper=p+margin
cbind(p,margin,lower,upper)

#The interval for the amout who would have voted SP is (6.7%,10.4%)

#The margins of error seems decrease for FrP and SP