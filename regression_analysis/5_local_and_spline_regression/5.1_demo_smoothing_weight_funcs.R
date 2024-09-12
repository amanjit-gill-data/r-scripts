# WEEK 5: LOESS SMOOTHER
# Demo on weighting functions

plot(NULL, main="LOESS Weighting Functions", xlim=c(-2, 2), ylim=c(0, 1),
     xlab="u", ylab="K(u)")

# Epanechikov
x1 <- seq(-1, 1, length.out = 100)
y1 <- 0.75 * (1 - x1^2)
lines(x1, y1, col=1, lwd=1.5)

# Box
x2 <- c(-1, -1, 1, 1)
y2 <- c(0, .5, .5, 0)
lines(x2, y2, col=2, lwd=1.5)

# Triangular
x3 <- c(-1, 0, 1)
y3 <- c(0, 1, 0)
lines(x3, y3, col=3, lwd=1.5)

# Tri-Cube
x4 <- seq(-1, 1, length.out = 100)
y4 <- (1 - abs(x4)^3)^3
lines(x4, y4, col=4, lwd=1.5)

# Gaussian
x5 <- seq(-1, 1, length.out = 100)
y5 <- dnorm(x5)
lines(x5, y5, col=5, lwd=1.5)

legend(
  "topleft",
  legend=c("Epanechikov", "Box", "Triangular", "Tri-Cube", "Gaussian"),
  lwd=1.5, col=c(1,3,4,5,6), cex=0.75, bty="n"
)
