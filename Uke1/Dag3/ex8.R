nb_dispensers = c(0,0,1,1,2,2,4,4,5,5,6,6,7,7)
Coffee_sales = c(508.1,498.4,568.2,577.3,651.7,657.0,755.3,758.9,787.6,792.1,841.4,832.8,854.7,871.4)

fit.lin = lm(Coffee_sales~nb_dispensers)
summary(fit.lin)


fit.poly = lm(Coffee_sales~nb_dispensers + I(nb_dispensers^2))
summary(fit.poly)

pred <- predict(fit.poly)

plot(nb_dispensers,Coffee_sales)
abline(fit.lin)
lines(nb_dispensers,pred)

#The Polynomial fit is clearly better!