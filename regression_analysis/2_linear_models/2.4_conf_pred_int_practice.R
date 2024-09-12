# 2.4 Confidence and Prediction Intervals - Example and Exercise

### EXAMPLE ###

syd <- read.table("sydney.txt", header = TRUE)
head(syd)

# step 1: fit model
lm.fit <- lm(Maxtemp ~ ., data = syd)

# step 2: confidence and prediction intervals

newdata <- data.frame (
  Modst=c(285.1,288.0), 
  Modsp=c(1021.2,1026.8), 
  Modthik=c(5380.4,5388.4)
)

predict(lm.fit, newdata, interval = "confidence") # or predict.lm()
predict(lm.fit, newdata, interval = "prediction") # or predict.lm()

# can also print out SE calcs
# will return same SE calcs for both conf and pred
predict(lm.fit, newdata, interval = "confidence", se.fit = T) # or predict.lm()
predict(lm.fit, newdata, interval = "prediction", se.fit = T) # or predict.lm()

### EXERCISE ###

# use randomly generated normal data
x <- rnorm(15)
y <- x + rnorm(15)

# step 1: fit model

lm.fit <- lm(y ~ x)

# step 2: predict values for new data, including conf and pred intervals

newdata <- data.frame (
  x = seq(-3, 3, 0.5)
)

y.hat <- predict(lm.fit, newdata)
ci <- predict(lm.fit, newdata, interval = "confidence")
pi <- predict(lm.fit, newdata, interval = "prediction")

# step 3: plot intervals using matplot()

# plot:
# regression line in black
# conf interval lines in blue
# pred interval lines in red
matplot(
  newdata$x, cbind(pi, ci[,-1]),
  lty=c(1,2,2,4,4), col=c(1,2,2,4,4),
  type = "l", ylab = "predicted y"
)

# add:
# predicted points in black, along regression line
# original data in red
points(newdata$x, y.hat, pch=16)
points(x, y, col=2, pch=16)


