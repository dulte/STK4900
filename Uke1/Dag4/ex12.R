gun=read.table("http://www.uio.no/studier/emner/matnat/math/STK4900/v17/gun.dat", col.names=c("method","phys","team","rounds"))


#a)
cor(gun)

#The rounds seems to be most correlated with method, which makes sense.
#Is is somewhat correlated with phys, since the phys of the teams makes
#loading easier
#Since the teams are random, we do not expect a large correlation with round

#The teams are independent, and have therefore no correlation eachother
#Each team did tried the methods, so no team is correlated with a method.
#All the teams had the same type of phys, so no correlation


#b)
gun$method=factor(gun$method)
gun$phys=factor(gun$phys)
gun$team=factor(gun$team)

gfit=lm(rounds~method*phys*team, data=gun) 
anova(gfit)

summary(gfit)

#Only the method seems to have a real significance.
# We can also see that phys has some effect, but not significant.
#There are no significant interactions, but phys:team is close.
#We can see from the summary that team 2 is actually significant, meaning
#that they did very poorly, which also may explain the interaction. 
#Looking at the interaction between phys3:team2 we see that it is almost
#significant

#But all in all the only significant covariant was the method, 
#so we can use the model: round = 22.15 - 6.95*method2
