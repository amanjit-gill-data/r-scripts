# 5.1 LOESS AND KERNEL SMOOTHERS
# activity using air dataset

air <- read.table("air.txt", header=TRUE)

# order the predictors so the lines joining the fitted values won't 
# criss-cross the plot
air <- air[order(air$radiation), ]

# fit loess and kernel smoothers
lfit <- loess(air$ozone ~ air$radiation, span = 0.75)
kfit <- ksmooth(air$radiation, air$ozone, bandwidth = 100)

plot(air$radiation, air$ozone, cex=0.75, main="Air")
lines(air$radiation, lfit$fitted, col="magenta")
lines(air$radiation, kfit$y, col="blue")

legend("topleft", legend=c("loess", "kernel"),
       bty="n", cex=0.75, lty=1, col=c("magenta", "blue"))
