# 5.4 SMOOTHING SPLINE - USING GCV
# demo using fossil data

fossil <- read.table("fossil.txt", header=TRUE)
attach(fossil)

ss1 <- smooth.spline(age, strontium.ratio)
ss2 <- smooth.spline(age, strontium.ratio, df=4)
ss3 <- smooth.spline(age, strontium.ratio, df=25)

x <- seq(min(age), max(age), length.out=200)
plot(age, strontium.ratio)
lines(x, predict(ss1, x)$y, col="red", lwd=2)
lines(x, predict(ss2, x)$y, col="magenta")
lines(x, predict(ss3, x)$y, col="blue")
