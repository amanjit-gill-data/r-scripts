# 5.2 POLYNOMIAL REGRESSION
# demo on different ways to enter polynomial variables

library(ISLR)
data("Wage")

# can use poly(raw=TRUE) to create the polynomial columns
pfit1 <- lm(wage ~ poly(age, 4, raw=TRUE), data=Wage)

# can use powers directly in lm() with I()
# I() stops lm() from interpreting mathematical operators in its own way
pfit2 <- lm(wage ~ age + I(age**2) + I(age**3) + I(age**4), data=Wage)

# can construct the polynomial columns myself
pfit3 <- lm(wage ~ cbind(age, age^2, age^3, age^4), data=Wage)

# compare all the coefficients
coef(summary(pfit1))
coef(summary(pfit2))
coef(summary(pfit3))
