hers=read.table("http://www.uio.no/studier/emner/matnat/math/STK4900/v17/hers.txt",sep="\t",header=T,na.strings=".")
hers$LDLch=hers$LDL1 - hers$LDL
hers$cLDL=hers$LDL-145


#a)
fit.a=lm(LDLch~LDL + HT, data=hers)
summary(fit.a)


#We see that the change in LDL is dependent on the entery level
#of LDL, with a p valeu of almost 0

#b)
fit.b=lm(LDLch~HT+cLDL, data=hers)
summary(fit.b)

#This gives exactly the same as in a), so centering the LDL
#does not alter the outcome (since it is a linear transformation)
#Only the intercept changed 

#c)
fit.c=lm(LDLch~HT+cLDL+cLDL:HT, data=hers)
summary(fit.c)


#This is a significant interaction!
#Those without HT is a decrease of 0.34771*cLDL during the first year.
#For those with HT the decrease was (0.34771+0.07869)*cLDL