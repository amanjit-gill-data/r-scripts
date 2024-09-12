# 5.3 REGRESSION SPLINE - LINEAR
# demo using lidar data

library(splines)
library(SemiPar) # just for lidar dataset

data("lidar")
attach(lidar)

# fit two linear splines; one with few knots, one with many knots
# bs() creates basis matrix i.e. design matrix for polynomial spline regression
fit1 <- lm(logratio ~ bs(x=range, knots=c(550, 600), degree=1)) # K = 2
fit2 <- lm(logratio ~ bs(x=range, df=51, degree=1)) # K = 51 - 1 = 50

plot(range, logratio, main="LIDAR", cex=0.5)

x <- seq(min(range), max(range), length.out=1000)
y1 <- predict(fit1, newdata=list(range=x))
y2 <- predict(fit2, newdata=list(range=x))

lines(x, y1, col="magenta", lwd=1.5)
lines(x, y2, col="blue", lwd=1.5)
