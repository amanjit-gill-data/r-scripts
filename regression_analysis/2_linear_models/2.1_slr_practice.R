# 2.1 Simple Linear Regression

# contains Boston dataset
library(MASS)

### EXAMINE DATASET ###

?Boston
names(Boston)
dim(Boston)
head(Boston)
summary(Boston$medv)

# with attach, can refer to columns without $
attach(Boston)
summary(cbind(medv, lstat))

# no need to attach/detach
with(data = Boston, summary(medv))

# plots
boxplot(cbind(medv, lstat), horizontal = TRUE)
plot(lstat, medv)

### FIT MODEL ###

lm.fit <- lm(medv ~ lstat, data=Boston)
summary(lm.fit)

# add to scatter plot
abline(lm.fit, col="red", lwd=3)

### CONFIDENCE INTERVALS FOR COEFF ESTIMATES - WITH MATH ###

# sigma.sq = variance of error = square of the RSE
sigma.sq <- function(model, y_vect) {
  
  y <- y_vect
  yhat <- fitted(model)
  n <- length(y_vect)
  
  RSS <- sum((y - yhat)**2)
  RSS / (n - 2)
}

# square of SE for beta0
SE.sq.beta0 <- function(x_vect, sigma.sq) {
  
  n <- length(x_vect)
  xbar <- mean(x_vect)
  
  sigma.sq * ((1/n) + xbar^2 / sum((x_vect-xbar)^2))
}

# square of SE for beta1
SE.sq.beta1 <- function(x_vect, sigma.sq) {
  
  xbar <- mean(x_vect)
  sigma.sq / sum((x_vect-xbar)^2)
}

# calcs for confidence intervals

sigma.hat.sq <- sigma.sq(lm.fit, medv)
SE.beta0 <- sqrt(SE.sq.beta0(lstat, sigma.hat.sq)) # 0.5626
SE.beta1 <- sqrt(SE.sq.beta1(lstat, sigma.hat.sq)) # 0.0387
sigma.hat <- sqrt(sigma.hat.sq) # 6.216

alpha <- 0.05
n <- length(lstat)
t.crit <- qt(alpha/2, df = n-2, lower.tail = FALSE) # 1.964682

CI.beta0.lower <- lm.fit$coefficients[1] - t.crit * SE.beta0 # 33.44846
CI.beta0.upper <- lm.fit$coefficients[1] + t.crit * SE.beta0 # 35.65922

CI.beta1.lower <- lm.fit$coefficients[2] - t.crit * SE.beta1 # -1.026148
CI.beta1.upper <- lm.fit$coefficients[2] + t.crit * SE.beta1 # -0.8739505

### CONFIDENCE INTERVALS FOR COEFF ESTIMATES - WITH BUILTIN FUNCTION ###

confint(lm.fit)


### HYPOTHESIS TEST FOR SLOPE - WITH MATH ###

# method 1 - compare t-statistic

t <- lm.fit$coefficients[2] / SE.beta1 # -24.5279; matches lm summary
t.crit <- qt(alpha/2, df = n-2, lower.tail = FALSE) # 1.964682 as before

if (abs(t) < t.crit) { print("Do not reject H0") } else { print("Reject H0") } 

# method 2 - compare P(t < t-statistic on LHS | t > t-statistic on RHS)
# calculate probability for one side, then double it
p <- 2 * pt(t, n-2)

if (p < alpha) { "Reject H0" } else { "Do not reject H0" }

### HYPOTHESIS TEST FOR SLOPE - WITH BUILTIN FUNCTION ###

# extract p-value for slope from lm object
# could just use the 2e-16 from the lm summary
# or can access the full value like this
p <- summary(lm.fit)$coefficients[,4][2]

if (p < alpha) { "Reject H0" } else { "Do not reject H0" }

### ASSESS OVERALL MODEL - WITH MATH ###

# RSE (this is just the square root of sigma.hat.sq i.e. sigma.hat)

RSE <- sigma.hat #6.216

# R^2

R.sq <- function(model, y_vect) {
  
  ybar <- mean(y_vect)
  TSS <- sum((y_vect - ybar)**2)
  
  yhat <- fitted(model)
  RSS <- sum((y_vect - yhat)**2)
  
  (TSS - RSS) / TSS
}

R.sq(lm.fit, medv) # 0.5441


### ASSESS OVERALL MODEL - WITH BUILTIN FUNCTION ###

# RSE from lm object
RSE <- summary(lm.fit)$sigma

# R^2 from lm object
R.sq <- summary(lm.fit)$r.squared

### DIAGNOSTIC PLOTS ###

# 1. scatter plot with regression line
# observation: non-linearity
plot(lstat, medv)
abline(lm.fit, col="red", lwd=3)

# 2. plots generated from lm object
# observations:
# resid vs fitted: pattern is evident -> non-linearity
# normal q-q: deviates from line
# sqrt(std resid) vs fitted: pattern is evident -> non-linearity
# std resid vs leverage: data are within acceptable range re cook's distance
plot(lm.fit)

# 3. hat-values vs index, with 3 x p/n threshold line
# observation: lots of high-leverage points

p <- 2 # no. parameters (beta0, beta1)
n <- length(lstat)

# method 1: use lm object
plot(hatvalues(lm.fit))
abline(h = 3*p/n, col="red")

# method 2: use x vector
plot(hat(lstat))
abline(h = 3*p/n, col="red")

# which data indices correspond to high leverage
# using both lm object and x vector

which.max(hatvalues(lm.fit)) # 375
which.max(hat(lstat)) #375

### PREDICTION ###

# uses data frame containing lstat column with desired predictors
predict(lm.fit, newdata = data.frame(lstat=c(5,10,15)))

# predictions with 95% confidence intervals
predict(lm.fit, newdata = data.frame(lstat=c(5,10,15)), interval = "confidence")

# predictions with 95% prediction intervals
predict(lm.fit, newdata = data.frame(lstat=c(5,10,15)), interval = "prediction")


detach(Boston)

