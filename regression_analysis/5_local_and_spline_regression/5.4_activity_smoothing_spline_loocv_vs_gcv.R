# 5.4 SMOOTHING SPLINE - GCV VS LOOCV
# activity using Wage dataset

library(ISLR)
data("Wage")
attach(Wage)

# 1. plot wage vs age

plot(age, wage, cex=0.5, main="Wage vs Age")

# 2. smoothing spline with df=16
# NO cross validation is done to determine df/lambda, as df is supplied

ss1 <- smooth.spline(age, wage, df=16)

# 3. smoothing spline using LOOCV

ss2 <- smooth.spline(age, wage, cv=TRUE) # use LOOCV by setting cv=TRUE

# 4. compare both fits

lines(ss1$x, ss1$y, col="magenta", lwd=2)
lines(ss2$x, ss2$y, col="blue", lwd=2)
legend("topright", legend=c("df=16", "LOOCV"),
       col=c("magenta", "blue"), lwd=2, cex=0.75, bty="n")

ss1$df
ss2$df

# OBSERVATIONS

# ss1 uses df=16
# ss2 selects df=6.79
# ss2 is thus less complex, and smoother, than ss1

# ss1 has cv=FALSE, so it uses GCV, but not to select df & lambda,
# as df is supplied
# ss2 has cv=TRUE, so it uses LOOCV, including to choose df & lambda,
# as df is not supplied




