# 3.2 Logistic Regression - Trade Union Example
# ungrouped pearson and deviance residuals (both maths and glm) 

library(SemiPar)

data("trade.union")
attach(trade.union)

### FIT MODEL ###

# using two predictors here
m <- glm(union.member ~ wage+age, family = "binomial")

### PEARSON RESIDUALS - USING MATHS ###

# probability of union membership, given a wage
prob <- function(b0, b1, b2, wage, age) {
  exp(b0 + b1*wage + b2*age) / (1 + exp(b0 + b1*wage + b2*age))
}

# coefficients of logistic regression model, needed for residual calcs
# same for grouped and ungrouped models
b0 <- m$coefficients[1]
b1 <- m$coefficients[2]
b2 <- m$coefficients[3]

# now use the formula to calculate pearson residuals
# the pearson resid formula is applied to every case in the dataset, like it's
# 534 groups of 1
num <- m$y - m$fitted.values
denom <- sqrt(m$fitted.values*(1 - prob(b0, b1, b2, wage, age)))
p.resid <- num/denom

# compare with pearson residuals from glm
(m.psq <- sum(residuals.glm(m, type = "pearson")**2))
(psq <- sum(p.resid**2)) # matches

### STANDARDISED PEARSON RESIDUALS - USING GLM OBJECT AND THEN MATHS ###

p.resid <- residuals.glm(m, type = "pearson")
hatvals <- hatvalues(m)

std.p.resid <- p.resid / sqrt(1 - hatvals)

### DEVIANCE RESIDUALS AND DEVIANCE - USING GLM OBJECT ###

dev.resids <- residuals.glm(m, type = "deviance")

(D <- sum(dev.resids**2))

# now see if it matches the glm deviance
m$deviance # matches

