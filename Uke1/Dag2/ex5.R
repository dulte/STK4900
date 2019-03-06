#a
pef = c(494,395,516,434,476,413,442,433)
minipef = c(512,430,520,428,500,364,380,445)
plot(pef,minipef)

#b
r = cor(pef,minipef)
r

#c
cor.test(pef,minipef)

#d
fit = lm(minipef~pef)
summary(fit)
plot(pef,minipef)
abline(fit)
#This gives minipef_i = -76.94 + 1.16*pef_i + error_i

#e
slope = 1.1642
r_slope = slope*sd(pef)/sd(minipef)
r_slope
#thus r = r_slope!