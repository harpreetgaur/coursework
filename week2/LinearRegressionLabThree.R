library(MASS)
library(ISLR)

#Viewing the data
fix(Boston)
names(Boston)

#fit linear regression model

#lm(y~x, data)
lm.fit=lm(medv~lstat, data=Boston)

attach(Boston)

lm.fit = lm(medv~lstat)

#Gives P values, R**2 and F stats
summary(lm.fit)

lm.fit

#To find out what other info is stored in lm.fit
names(lm.fit)

coef(lm.fit)

#confidence intervals
confint(lm.fit)

#predict can be used to produce conf intervals
predict (lm.fit,data.frame(lstat=c(5,10 ,15)),interval ="confidence")



predict (lm.fit,data.frame(lstat=c(5,10 ,15)),interval ="prediction")


lm.fit= lm(medv???lstat+age, data=Boston)
summary (lm.fit)




