poissontab=function(counts,x=(0:(length(counts)-1)))
{
    n=sum(counts)
    meanx=sum(x*counts)/n
    varx=sum((x-meanx)^2*counts)/(n-1)
    CD=varx/meanx
    df=length(counts)-2
    px=c(dpois(0:df,meanx),1-ppois(df,meanx))
    EX=n*px
    tab=cbind(x,counts,round(EX,2),round(counts-EX,2),round((counts-EX)^2/EX,2))
    X2=sum((counts-EX)^2/EX)
    pval=1-pchisq(X2,df)
    print(paste("Mean=",round(meanx,4)))
    print(paste("Var= ",round(varx,4)))
    print(paste("CD= ",round(CD,4)))
    tab=as.data.frame(tab)
    names(tab)=c("x","Obs","Exp","O-E","(O-E)^2/E")
    print(tab)
    print(paste("Pearson X2 =",round(X2,2)))
    print(paste("p-value=",round(pval,4)))
}

yeast=c(75,103,121,54,30,13,4)
poissontab(yeast)
