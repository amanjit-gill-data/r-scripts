# 4.4 VARIABLE SELECTION - BEST SUBSET SELECTION
# demo on regsubsets() using hitters data

library(ISLR)
library(leaps)
names(Hitters)
dim(Hitters)

# response = Salary
# explanatory = any combo of the remaining 19 variables

# CLEAN DATA

# check for NAs in RV and remove
sum(is.na(Hitters$Salary))
Hitters <- na.omit(Hitters)
dim(Hitters)

# check for NAs elsewhere
sum(is.na(Hitters))

# IDENTIFY BEST MODELS AT EVERY SIZE - DEFAULT MAX VARIABLES = 8

regfit <- regsubsets(Salary ~ ., data=Hitters)
summary(regfit)

# IDENTIFY BEST MODELS AT EVERY SIZE - SET MAX VARIABLES TO 19 (ALL)

regfit <- regsubsets(Salary ~ ., data=Hitters, nvmax = 19)
sub.summary <- summary(regfit)

# can extract useful information from fit object
sub.summary$rsq
sub.summary$rss
sub.summary$adjr2
sub.summary$cp # mallow's Cp
sub.summary$bic

# PLOT MEASURES OF FIT AND CHOOSE BEST MODEL SIZE

par(mfrow=c(2,2))
plot(sub.summary$rss, xlab="Model Complexity", ylab="RSS", type="l")
plot(sub.summary$adjr2, xlab="Model Complexity", ylab="Adjusted R^2", type="l")
plot(sub.summary$cp, xlab="Model Complexity", ylab="Cp", type="l")
plot(sub.summary$bic, xlab="Model Complexity", ylab="BIC", type="l")

# based on the plots, select model size = 6, due to lowest BIC
# find out which 6 variables are selected at this size

coef(regfit, 6)

