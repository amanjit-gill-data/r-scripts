# 5.2 POLYNOMIAL REGRESSION - SELECT DEGREE
# activity using Wage dataset

library(ISLR)
library(boot)
data("Wage")
attach(Wage)

# FIT 10 MODELS AND USE 10-FOLD CROSS VALIDATION TO ESTIMATE ERROR

d <- 10 # try up to degree 10
k <- 10 # do 10 folds for each degree
cv.errors <- vector(length = d)

set.seed(1) # to keep results the same every time
for (i in 1:d) {
  pfit <- glm(wage ~ poly(age, i, raw=TRUE)) # must be glm() to work with cv.glm()
  cv.errors[i] <- cv.glm(Wage, pfit, K=k)$delta[1] # only keep the raw error
}

# PLOT ESTIMATED ERRORS AND SELECT DEGREE

plot(x=1:10, y=cv.errors, type="o", lwd=2, pch=16, col="blue")

which.min(cv.errors)

# results:
# plot shows big drop in error from models 1 to 2
# then error from models 2 to 10 stays similar
# so for parsimony, choose degree 2, as degrees 3-10 don't show much/any improvement
# however, true minimum CV occurs for model 9
# so if parsimony isn't an imperative, we could choose degree 9

# solution chooses degree 9, so we'll fit our final model with that
m1 <- glm(wage ~ poly(age, 9, raw=TRUE))

# FIT THE 10 MODELS NORMALLY, USING LM(), SO WE CAN USE ANOVA

fit1 <- lm(wage ~ poly(age, 1, raw=TRUE))
fit2 <- lm(wage ~ poly(age, 2, raw=TRUE))
fit3 <- lm(wage ~ poly(age, 3, raw=TRUE))
fit4 <- lm(wage ~ poly(age, 4, raw=TRUE))
fit5 <- lm(wage ~ poly(age, 5, raw=TRUE))
fit6 <- lm(wage ~ poly(age, 6, raw=TRUE))
fit7 <- lm(wage ~ poly(age, 7, raw=TRUE))
fit8 <- lm(wage ~ poly(age, 8, raw=TRUE))
fit9 <- lm(wage ~ poly(age, 9, raw=TRUE))
fit10 <- lm(wage ~ poly(age, 10, raw=TRUE))

# DO ANOVA AND SELECT DEGREE

anova(fit1, fit2, fit3, fit4, fit5, fit6, fit7, fit8, fit9, fit10)

# results:
# from models 1 to 2, small p
# from models 2 to 3, small p
# from models 3 to 4, large p
# therefore there is little to be gained from degree 4
# so choose degree 3

# fit our final model with chosen degree 3
m2 <- lm(wage ~ poly(age, 3, raw=TRUE))

# PLOT BOTH CHOSEN MODELS (2 and 9) OVER THE ORIGINAL DATA

# generate new x-values in increasing order, so points join together properly
x <- seq(min(age), max(age), length.out = 1000)

# now predict y-values, using both models
y1 <- predict(m1, newdata=list(age = x))
y2 <- predict(m2, newdata=list(age = x))

plot(age, wage, cex=0.5)
matlines(x, cbind(y1, y2), lwd=2, lty=1, col=c("magenta", "blue"))

# observations:
# model 9 wobbles in an area of sparcity
# it looks like an overfit
# i'd be inclined to choose the lower-order model of degree 3

