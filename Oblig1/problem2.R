blood <- read.table("http://www.uio.no/studier/emner/matnat/math/STK4900/v19/mandatory/blood.txt",sep=",",header=TRUE)

#a)
summary(blood)
summary(subset(blood,alder==1))
summary(subset(blood,alder==2))
summary(subset(blood,alder==3))

boxplot(blodtr~alder,data=blood,ylab="Blood Pressure [mmHg]",xlab="Age Group",col=topo.colors(3))
legend("topleft",inset=.0,title="Age Group",c("30-45 Years","46-59 Years", "60-75 Years"),
       fill=topo.colors(3),horiz=T,cex=0.6)


#We can clearly see that there is a difference in the median
#of the three age groups, with increasing age leading to increase systolic blood pressure
#The middle age group is quite wide, and overlaps alot with the two other.


#b)
blood$alder=factor(blood$alder)
blood.anova = aov(blodtr~alder,data = blood)
summary(blood.anova)

#With a p-value of 0.00426 we see that there is a significant
#difference between the two groups, as we see in the bloxplot



#c)
blood.fit = lm(blodtr~alder,data = blood)
summary(blood.fit)

#We see that is some linear relationship between the ages. 
#We also see that the mean age of the youngest age group is 122.167.
#If you are in the middle age group this increases by 16.9, 
# and in the oldest age groupe it increases with 33.

#The increase of 33 for the oldest age group is significant, 
# but for the middle age group we only have a p-value of 0.074,
# which means that we cannot say that it is significant.
# The reason might be the large spread of the blood pressures of
# This age group.

# The p-value of the whole linear model is the same as for the ANOVA,
# so it is a significant linear model.
