# 3.2 Logistic Regression - Trade Union Example
# pearson chi-squared and deviance statistics; use deviance in hypothesis test

library(SemiPar)
library(doBy)

data("trade.union")
attach(trade.union)

### FIT MODEL ###

m <- glm(union.member ~ wage+age, family = "binomial")
summary(m)

### PEARSON CHI-SQUARED STATISTIC ###

# manually computed pearson residuals elsewhere for similar problem
p.resid <- residuals.glm(m, type = "pearson")

psq.stat <- sum(p.resid**2)

### DEVIANCE STATISTIC - USING SEPARATE GLMS ###

m1 <- m # model of interest
m0 <- glm(union.member ~ 1, family = "binomial") # null model

# hypothesis test; H0 is that slopes b1 and b2 are zero

d0 <- m0$deviance
d1 <- m1$deviance
DeltaD <- d0 - d1

alpha <- 0.05
df <- length(m1$coefficients) - length(m0$coefficients) # p - q
D.crit <- qchisq(alpha, df, lower.tail = FALSE)

if (DeltaD > D.crit) { print("Reject H0") } else { print("Do not reject H0") }

### DEVIANCE STATISTIC - USING MODEL OF INTEREST ONLY ###

# model of interest has both d0 and d1
d0 <- m$null.deviance
d1 <- m$deviance

# confirm same as previous
DeltaD <- d0 - d1

### DEVIANCE STATISTIC - USING MATHS ###

# don't know how to do this with two predictors i.e. 3-way



