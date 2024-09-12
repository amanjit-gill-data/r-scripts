# 3.2 Logistic Regression - Trade Union Example
# other goodness of fit statistics
# likelihood ratio, pseudo-R^2, AIC, BIC

library(SemiPar)
data("trade.union")
attach(trade.union)

### LIKELIHOOD RATIO STATISTIC ###

# method 1: fit M0 and M1, then use loglik() to compute statistic

m1 <- glm(union.member ~ wage+age, family = "binomial")
m0 <- glm(union.member ~ 1, family = "binomial")

# logl statistic
C.stat <- 2 * ( logLik(m1) - logLik(m0))

# critical value
alpha <- 0.05
df <- 3 - 1 # difference in no. parameters between M1 and M0
C.crit <- qchisq(alpha, df, lower.tail = FALSE)

# compare
# note that C.stat is an object; the number needs to be extracted from it
if (C.stat[1] > C.crit) { print("Reject H0") } else { print("Do not reject H0") }

### PSEUDO-R^2 ###

# use M0 and M1

pseudo.Rsq <- (logLik(m0) - logLik(m1)) / logLik(m0)
pseudo.Rsq <- pseudo.Rsq[1]

### AIC AND BIC ###

# compute separately for each model, then compare
# lower is better
# result: AIC and BIC are lower for the most complex model, unsurprisingly

m0 <- glm(union.member ~ 1, family = "binomial")
m1 <- glm(union.member ~ wage, family = "binomial")
m2 <- glm(union.member ~ wage + age, family = "binomial")

# method 1 - using maths

p <- 3 # biggest model has 3 parameters
N <- nrow(trade.union)

(AIC.0 <- -2*logLik(m0) + 2*(p-2))[1]
(AIC.1 <- -2*logLik(m1) + 2*(p-1))[1]
(AIC.2 <- -2*logLik(m2) + 2*p)[1]

(BIC.0 <- -2*logLik(m0) + (p-2)*log(N))[1]
(BIC.1 <- -2*logLik(m1) + (p-1)*log(N))[1]
(BIC.2 <- -2*logLik(m2) + p*log(N))[1]

# method 2 - using glm object with AIC and BIC formulas

(AIC.0 <- AIC(m0))
(AIC.1 <- AIC(m1))
(AIC.2 <- AIC(m2))

(BIC.0 <- BIC(m0))
(BIC.1 <- BIC(m1))
(BIC.2 <- BIC(m2))




