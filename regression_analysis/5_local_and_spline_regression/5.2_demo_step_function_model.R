# 5.2 FIT A STEP FUNCTION
# demo using Wage data

library(ISLR)
data("Wage")
attach(Wage)

# age range is 18 to 80
# cut(age, 4) divides the age range into 4 bins, and replaces each age with 
# its allocated bin; so we've converted the ages into an ordinal categorical 
# variable, with 4 possible categories
head(cut(age, 4))

# can use table() to see how many ages belong to each bin
table(cut(age, 4))

# fit the vector of 3000 categorised predictors to a stepwise function
# this is stepwise because all the ages within a certain range have the same 
# bin (value/ordinal category), and will have the same 
# *constant* predicted response
sfit <- lm(wage ~ cut(age, 4))

coef(summary(sfit))

# visualise what these steps look like against the original data
# start by generating a sequence of ordered x values to ensure the points 
# join together properly
x <- seq(min(age), max(age), length.out = 200)
y <- predict(sfit, newdata= list(age=x))

plot(age, wage, ylim=c(0, 200)) # limit y-scale to make the four steps clearer
lines(x, y, lwd=2, col="magenta")

