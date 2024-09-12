# 5.3 REGRESSION SPLINE - NATURAL
# demo using Wage dataset

library(splines)
library(ISLR)
data(Wage)
attach(Wage)

# fit both a natural and ordinary spline; cubic by default
fit.nat <- lm(wage ~ ns(age, df=4)) # K = df + 1 = 4 + 1 = 5; degree 3 always
fit.ord <- lm(wage ~ bs(age, df=4)) # K = df - 3 = 4 - 3 = 1; degree 3 by default

x <- seq(min(age), max(age), length.out=500)
y.nat <- predict(fit.nat, newdata=list(age=x))
y.ord <- predict(fit.ord, newdata=list(age=x))

plot(age, wage, cex=0.5)
lines(x, y.nat, col="magenta", lwd=1.5)
lines(x, y.ord, col="blue", lwd=1.5)


