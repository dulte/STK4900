solvents=read.table("http://www.uio.no/studier/emner/matnat/math/STK4900/data/solvents.txt",header=T)

#summary(solvents)

# Boxplots seems to show that there is some difference on sorption rates
boxplot(rate~type,data=solvents)

solvents$type=factor(solvents$type)


aov.solvents = aov(rate~type,data=solvents)
summary(aov.solvents)
