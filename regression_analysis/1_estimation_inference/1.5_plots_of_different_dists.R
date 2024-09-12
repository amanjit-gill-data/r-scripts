# 1.5 Binomial, Normal, Poisson Distributions

# binomial
# x = integer = no. successful trials out of 40
x <- seq(0,40)
n <- 40
p <- 0.5
plot(x, dbinom(x, n, p), type="l")

# normal
# set x values to be symmetrical around mu so it looks nice
# i.e. from 0 to 2*mu, so mu is in the middle
mu <- 0 
sd <- 1
xtest <- seq(-3, 3, 0.1)
plot(xtest, dnorm(xtest, mu, sd), type="l")
abline(v = c(-1.32, 1.32))
abline(v = 1.96)

# poisson
# x = integer = counts per interval/item
x <- seq(0,10)
lambda <- 1
plot(x, dpois(x, lambda), pch=18)

# student-t
x <- seq(-10, 10, 0.1)
df <- 398
plot(x, dt(x, df), type="l")
abline(v = -7.2)

# F
x <- seq(0, 30, 0.1)
df1 <- 2
df2 <- 69
plot(x, df(x, df1, df2), type="l")
abline(v = 20)
abline(v=25)

# chi-squared
xtest <- seq(0, 20, 0.1)
df <- 1
plot(xtest, dchisq(xtest, df), type="l")
abline(v = 14.7603)
abline(v = 3.841459)
