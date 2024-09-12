# 2.2 Linear Gaussian Models

mitsub <- read.table("mitsub.txt", header = TRUE)
attach(mitsub)

head(mitsub)
n <- nrow(mitsub)

### ESTIMATE COEFFS USING MLE FORMULA ###

# need:
# X design matrix
# y responses

X <- cbind(rep(1, n), age)
y <- price

(coeff.est <- solve(t(X) %*% X) %*% t(X) %*% y)

### ESTIMATE COEFFS USING LM FUNCTION ###

lm.fit <- lm(price ~ age)
lm.fit$coefficients

### ESTIMATE COEFFS USING GLM FUNCTION ###

glm.fit <- glm(price ~ age, family="gaussian")
glm.fit$coefficients

### SCATTER PLOT WITH REGRESSION LINE ###

# can use the lm/glm object...
plot(age, price)
abline(lm.fit, lwd=2, col="red")

# ...or can use the fitted values directly, joining them with short lines
plot(age, price)
lines(x = age, y = lm.fit$fitted.values, lwd=2, col="red")

### CONFIDENCE INTERVALS FOR PARAMETERS ###

# denominator (n-2) can also be given by lm.fit$df.residual
sigma.hat <- sqrt(sum(summary(lm.fit)$residuals^2)/(n-2))
xbar <- mean(age)

SE.beta0 <- sqrt(sigma.hat^2 * ( 1/n + xbar^2 / sum((age - xbar)^2)))
SE.beta1 <- sqrt(sigma.hat^2 / sum((age - xbar)^2))

alpha <- 0.05
t_crit <- qt(alpha/2, n-2, lower.tail = FALSE)

beta0.hat <- lm.fit$coefficients[1]
beta1.hat <- lm.fit$coefficients[2]

(CI.beta0.lower <- beta0.hat - t_crit*SE.beta0)
(CI.beta0.upper <- beta0.hat + t_crit*SE.beta0)
(CI.beta1.lower <- beta1.hat - t_crit*SE.beta1)
(CI.beta1.upper <- beta1.hat + t_crit*SE.beta1)

# check against builtin function
confint(lm.fit)

detach(mitsub)
