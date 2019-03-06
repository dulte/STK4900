weight.Virginia = c(13.1,12.4,13.2,11.8)
age.Virginia = c(29,27,28,26)

weight.Wisconsin = c(11.5,14.2,15.4,13.1,13.8)
age.Wisconsin = c(21,27,29,23,25)



plot(age.Wisconsin,weight.Wisconsin,col="green")
points(age.Virginia,weight.Virginia,col="red")

#There is a clear difference in the plots
#This means that we need to consider where the turkey is from!