# 6.1 GENERALISED ADDITIVE MODEL - LOGISTIC REGRESSION
# demo using Wages data

# predict whether or not someone earns over 250K

library(ISLR)
library(gam)

data(Wage)
attach(Wage)

# wage > 250 converts all the wages to T/F
# I() stops > from being interpreted differently by gam()
# remember to use family=binomial, for logistic regression
lr.gam1 <- gam(I(wage > 250) ~ year + s(age, df=5) + education, family=binomial)

plot.Gam(lr.gam1, se=TRUE)

# SE band for the "< HS Grad" level is very large, indicating very few data
# points for this level
# investigate by creating a contingency table
table(education, I(wage > 250))

# contingency table result:
# there are NO people who earn over 250K, whose education level is "< HS Grad"
# therefore we can exclude this education level from the analysis by adding a
# 'subset' argument to gam()

lr.gam2 <- gam(I(wage > 250) ~ year + s(age, df=5) + education, family=binomial,
               subset = (education != "1. < HS Grad"))

plot.Gam(lr.gam2, se=TRUE)

