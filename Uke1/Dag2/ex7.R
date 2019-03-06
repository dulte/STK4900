library(MASS)

#a)
n=25
rho=0.30
m=matrix(c(0,0),nrow=2)
S=matrix(c(1,rho,rho,1),nrow=2)
obs=mvrnorm(n,m,S)
x=obs[,1]
y=obs[,2]
cor(x,y)
plot(x,y)

#b)
n=25
rho=0.90
m=matrix(c(0,0),nrow=2)
S=matrix(c(1,rho,rho,1),nrow=2)
obs=mvrnorm(n,m,S)
x=obs[,1]
y=obs[,2]
cor(x,y)
plot(x,y)

#c)
n=1000
rho=0.30
m=matrix(c(0,0),nrow=2)
S=matrix(c(1,rho,rho,1),nrow=2)
obs=mvrnorm(n,m,S)
x=obs[,1]
y=obs[,2]
cor(x,y)
plot(x,y)