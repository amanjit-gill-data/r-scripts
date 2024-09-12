# 2.2 Assessing Model Assumptions - Practice

# use mitsub.txt to test out formulas

mit <- read.table("mitsub.txt")
attach(mit)
head(mit)
n <- nrow(mit)

# design matrix - before fitting: manually add a column of 1s
X <- cbind(rep(1, n), age) # doesn't work unless you 'attach'

# design matrix - after fitting: can use lm object
X <- model.matrix(lm(price ~ age))

# responses
y <- price

# hat matrix
H <- X %*% solve(t(X) %*% X) %*% t(X)

### ESTIMATE Y ###

# use H to estimate y
y.hat <- H %*% y

# compare against lm object
lm.fit <- lm(price ~ age)
lm.fit$fitted.values

### RESIDUALS ### 

# either find them with y - y.hat...
resid <- price - lm.fit$fitted.values

# or take them from the lm output
resid <- lm.fit$residuals

# plot residuals vs either x or y.hat, with zero line
plot(age, resid)
abline(h = 0)

# plot residuals vs y.hat using lm object; it's plot 1 out of 4
plot(lm.fit)

### LEVERAGE OF FITTED VALUES ###

# using hat matrix
diag(H)

# using design matrix
hat(X)

# find indices of points with high leverage
age[which(hat(X) >= 3*2/length(age))]

### STANDARDISED RESIDUALS ###

sigma.hat.sq <- sum(lm.fit$residuals^2) / (lm.fit$df.residual)
resid.std <- resid / sqrt (sigma.hat.sq * (1-diag(H)))

# plot standardised residuals vs fitted values
# with lines to mark +/- 2 standard deviations from std normal mean (0)
plot(lm.fit$fitted.values, resid.std)
abline(h = c(-2, 0, 2), lty = c(2,1,2))

### COOK'S DISTANCE FOR FITTED VALUES ###

# from lm object
cook.dist <- cooks.distance(lm.fit) 

# normalise
cook.dist <- cook.dist / max(cook.dist)

# plot data, and shade points according to cook's distance
# very dark i.e. black --> high cook's distance
cook.colours <- gray(1 - sqrt(cook.dist))
plot(age, price, bg=cook.colours, pch=21)
abline(lm.fit, col="red")

detach(mit)