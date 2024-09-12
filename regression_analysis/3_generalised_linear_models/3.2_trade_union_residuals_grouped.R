# 3.2 Logistic Regression - Trade Union Example
# grouped pearson residuals (both maths and glm) 

library(SemiPar)

data("trade.union")
attach(trade.union)

### CONTINGENCY TABLE - REQUIRED FOR GROUPED DATA ###

# workers on each wage
worker_counts <- aggregate(wage, by=list(wage), length)

# workers on each wage, who are union members
member_counts <- aggregate(union.member, by=list(wage), sum)

# contingency table
# x = wage
# y = union members on that wage
# n = total workers on that wage
ct <- cbind(member_counts, worker_counts$x)
names(ct) <- c("x", "y", "n")
head(ct)

# note that for grouped data:
# the 'formula' argument is different
# the 'data' argument is the contingency table, not the original dataset
m <- glm(cbind(y, n - y) ~ x, data = ct, family = "binomial")

# probability of union membership, given a wage
prob <- function(b0, b1, wage) {
  exp(b0 + b1*wage) / (1 + exp(b0 + b1*wage))
}

# expected number of union members, given group size and probability
exp_freq <- function(n, p) {
  n*p
}

# coefficients of logistic regression model, needed for residual calcs
# same for grouped and ungrouped models
b0 <- m$coefficients[1]
b1 <- m$coefficients[2]

# numerator and denominator of Pearson residuals
# one for each wage i.e. group so there are 238 Pearson residuals
num <- ct$y - exp_freq(ct$n, prob(b0, b1, ct$x))
denom <- sqrt(exp_freq(ct$n, prob(b0, b1, ct$x))*(1-prob(b0, b1, ct$x)))
p.resid <- num/denom

# compare with pearson resids from glm
(m.psq <- sum(residuals.glm(m, type = "pearson")**2))
(psq <- sum(p.resid**2)) # matches


