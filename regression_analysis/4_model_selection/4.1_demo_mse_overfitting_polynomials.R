# WEEK 4 - DEMO - MSE AND OVERFITTING

set.seed(202105)

x <- runif(20) # generate 20 numbers between 0 and 1
err <- rnorm(20, sd=0.25) # generate 20 errors with mean 0 and sd 0.25
y <- x + err

# fit polynomials of order k= 5, 10, 15
poly5 <- lm(y ~ poly(x, 5))
poly10 <- lm(y ~ poly(x, 10))
poly15 <- lm(y ~ poly(x, 15))

# generate 50 evenly spaced x values
new.x <- seq(min(x), max(x), length=50)

# use polynomial models to predict y values for the 50 x values
predict.p5 <- predict(poly5, newdata=list(x = new.x))
predict.p10 <- predict(poly10, newdata=list(x = new.x))
predict.p15 <- predict(poly15, newdata=list(x = new.x))

# plot randomly generated data points  
plot(x, y, pch=16, ylim=range(predict.p10), xlab="x", ylab="y")

# plot points predicted by each polynomial model
points(new.x, predict.p5, type="l", col="blue", lwd=2)
points(new.x, predict.p10, type="l", col="green", lwd=2)
points(new.x, predict.p15, type="l", col="red", lwd=2)



