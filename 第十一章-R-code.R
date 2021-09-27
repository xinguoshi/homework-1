# Reference: www.statlearning.com
# Week 11 Lab: Multiple Regression

# Simple Linear Regression
# The MASS library contains the Boston  data set, which records medv  (median house value) for 506 neighborhoods around Boston. We will seek to predict medv  using 13 predictors such as rm  (average number of rooms per house), age  (average age of houses), and lstat  (percent of households with low socioeconomic status).

#######################################################################

library(MASS)
library(ISLR)

attach(Boston)
lm.fit=lm(medv~lstat)
par(mfrow=c(2,2))
plot(lm.fit)
plot(predict(lm.fit), residuals(lm.fit))
plot(predict(lm.fit), rstudent(lm.fit))
plot(hatvalues(lm.fit))
which.max(hatvalues(lm.fit))

# Multiple Linear Regression

lm.fit=lm(medv~lstat+age,data=Boston)
summary(lm.fit)
lm.fit=lm(medv~.,data=Boston)
summary(lm.fit)
?summary.lm
summary(lm.fit)$r.sq
summary(lm.fit)$sigma

library(car)
vif(lm.fit)
lm.fit1=lm(medv~.-age,data=Boston)
summary(lm.fit1)
lm.fit1=update(lm.fit, ~.-age)

# Interaction Terms
summary(lm(medv~lstat+age+lstat:black,data=Boston))
summary(lm(medv~lstat*age,data=Boston))

# Non-linear Transformations of the Predictors

lm.fit2=lm(medv~lstat+I(lstat^2))
summary(lm.fit2)
lm.fit=lm(medv~lstat)
anova(lm.fit,lm.fit2)

par(mfrow=c(2,2))
plot(lm.fit2)


# Qualitative Predictors

fix(Carseats)
names(Carseats)
attach(Carseats)
class(ShelveLoc)
lm.fit=lm(Sales~.+Income:Advertising+Price:Age,data=Carseats)
summary(lm.fit)
attach(Carseats)
contrasts(ShelveLoc)

