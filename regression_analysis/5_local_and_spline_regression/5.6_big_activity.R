# 5.6 ACTIVITY

library(ISLR)
library(splines)
data("Wage")
attach(Wage)

# 1. REGRESSION SPLINE WITH SUPPLIED KNOTS

bs.fit1 <- lm(wage ~ bs(age, knots = c(25, 40, 60)))

x <- seq(min(age), max(age), length.out = 100)
y <- predict(bs.fit1, newdata=list(age = x), se = TRUE)

plot(age, wage, cex=0.5)
lines(x, y$fit, lwd=2, col="magenta")
lines(x, y$fit - 2*y$se.fit, col="magenta", lty=2)
lines(x, y$fit + 2*y$se.fit, col="magenta", lty=2)

# 2. REGRESSION SPLINE WITH SUPPLIED DF AND DEGREE M

bs.fit2 <- lm(wage ~ bs(age, df=6, degree=2))

y <- predict(bs.fit2, newdata=list(age = x), se = TRUE)

plot(age, wage, cex=0.5)
lines(x, y$fit, lwd=2, col="magenta")
lines(x, y$fit - 2*y$se.fit, col="magenta", lty=2)
lines(x, y$fit + 2*y$se.fit, col="magenta", lty=2)

# find locations of knots

attr(bs(age, df=6, degree=2), "knots")

# 3. NATURAL CUBIC SPLINE WITH SUPPLIED DF

ns.fit <- lm(wage ~ ns(age, df=6))

# compare with first model

y.bs <- predict(bs.fit1, newdata=list(age = x), se=TRUE)
y.ns <- predict(ns.fit, newdata=list(age = x), se=TRUE)

plot(age, wage, cex=0.5)

lines(x, y.bs$fit, lwd=2, col="magenta")
lines(x, y.bs$fit - 2*y.bs$se.fit, lty=2, col="magenta")
lines(x, y.bs$fit + 2*y.bs$se.fit, lty=2, col="magenta")

lines(x, y.ns$fit, lwd=2, col="blue")
lines(x, y.ns$fit - 2*y.ns$se.fit, lty=2, col="blue")
lines(x, y.ns$fit + 2*y.ns$se.fit, lty=2, col="blue")

# observation:
# natural spline has less variability at boundary

# 4. SMOOTHING SPLINES

# supply df=16; CV is not used to choose df
s.fit1 <- smooth.spline(age, wage, df=16)

# supply cv=TRUE to use LOOCV to choose df
s.fit2 <- smooth.spline(age, wage, cv=TRUE)

plot(age, wage, cex=0.5)
lines(s.fit1, lwd=2, col="magenta")
lines(s.fit2, lwd=2, col="blue")

s.fit2$df

# observation:
# df=16 is more complex, and more variable (wiggly)
# cv=TRUE chooses a simpler df=6.79, and is thus less variable
