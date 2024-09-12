# 5.1 LOESS SMOOTHER
# activity using ethanol dataset

eth <- read.table("ethanol.txt", header=TRUE)

# order the predictors so the lines joining the fitted values won't 
# criss-cross the plot
eth <- eth[order(eth$E), ]

plot(eth$NOx ~ eth$E, main="Ethanol", cex=.75)

# try a quadratic polynomial fit
polyfit <- lm(eth$NOx ~ poly(eth$E, 2, raw=TRUE))
lines(eth$E, polyfit$fitted.values, lwd=1, col="orange")

# try a loess smoother with default span=0.75
lfit1 <- loess(eth$NOx ~ eth$E)
lines(eth$E, lfit1$fitted, lwd=1, col="blue")

# try a loess smoother with smaller span=0.2
lfit2 <- loess(eth$NOx ~ eth$E, span=0.2)
lines(eth$E, lfit2$fitted, lwd=1, col="magenta")

# observations:
# polynomial regression raises the troughs and truncates the peaks
# loess smoother with span=0.7 captures the troughs and peaks a bit better
# loess smoother with span=0.2 captures the troughs and peaks much better

legend("topleft", 
       legend=c("quadratic polynomial", "loess - large span", "loess - small span"),
       bty="n", col=c("orange", "blue", "magenta"), lty=1, cex=0.75)


