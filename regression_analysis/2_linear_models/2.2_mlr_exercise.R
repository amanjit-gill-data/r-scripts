# 2.2 Linear Gaussian Models - Exercise

bb <- read.table("basketball.txt", header=TRUE)
attach(bb)

head(bb)
n <- nrow(bb)

### ESTIMATE COEFFS USING MLE FORMULA ###

# need:
# X design matrix
# y responses

X <- cbind(rep(1, n), Height, APM, MPG, Age)
y <- PPM

(coeff.est <- solve(t(X) %*% X) %*% t(X) %*% y)

### ESTIMATE COEFFS USING LM FUNCTION ###

lm.fit <- lm(PPM ~ Height + APM + MPG + Age)
lm.fit$coefficients

### ESTIMATE COEFFS USING GLM FUNCTION ###

glm.fit <- glm(PPM ~ Height + APM + MPG + Age, family = "gaussian")
glm.fit$coefficients

### CONFIDENCE INTERVALS FOR PARAMETERS - USING MATH ###

params = 5
sigma.hat <- sqrt(sum(summary(lm.fit)$residuals^2)/(n-params))

cov.est <- sigma.hat^2 * solve(t(X) %*% X)
SE.est <- sqrt(diag(cov.est)) # vector of SEs for all parameters

alpha <- 0.05
t_crit <- qt(alpha/2, n-2, lower.tail = FALSE)

beta.est <- lm.fit$coefficients # vector of estimates for all parameters

# CI for beta0
(CI.beta0.lower <- beta.est[1] - t_crit*SE.est[1])
(CI.beta0.upper <- beta.est[1] + t_crit*SE.est[1])

# CI for beta1
(CI.beta1.lower <- beta.est[2] - t_crit*SE.est[2])
(CI.beta1.upper <- beta.est[2] + t_crit*SE.est[2])

# CI for beta2
(CI.beta2.lower <- beta.est[3] - t_crit*SE.est[3])
(CI.beta2.upper <- beta.est[3] + t_crit*SE.est[3])

# CI for beta3
(CI.beta3.lower <- beta.est[4] - t_crit*SE.est[4])
(CI.beta3.upper <- beta.est[4] + t_crit*SE.est[4])

# CI for beta4
(CI.beta4.lower <- beta.est[5] - t_crit*SE.est[5])
(CI.beta4.upper <- beta.est[5] + t_crit*SE.est[5])

### CONFIDENCE INTERVALS FOR PARAMETERS - USING FUNCTION ### 

confint(lm.fit)

### HYPOTHESIS TESTING FOR PARAMETERS - COMPARING T-STATS ###

# calculate t-statistic for each coefficient
# make sure they match the t-stats in lm output

(t.beta0 <- beta.est[1] / SE.est[1])
(t.beta1 <- beta.est[2] / SE.est[2])
(t.beta2 <- beta.est[3] / SE.est[3])
(t.beta3 <- beta.est[4] / SE.est[4])
(t.beta4 <- beta.est[5] / SE.est[5])

# compare against t_crit

if (abs(t.beta0) < t_crit) { print("Do not reject H0") } else { print("Reject H0") }
if (abs(t.beta1) < t_crit) { print("Do not reject H0") } else { print("Reject H0") }
if (abs(t.beta2) < t_crit) { print("Do not reject H0") } else { print("Reject H0") }
if (abs(t.beta3) < t_crit) { print("Do not reject H0") } else { print("Reject H0") }
if (abs(t.beta4) < t_crit) { print("Do not reject H0") } else { print("Reject H0") }

### HYPOTHESIS TESTING FOR PARAMETERS - COMPARING P-VALUES ###

# calculate p-value for each coefficient
# make sure they match the p-values in lm output

p.beta0 <- 2 * pt(t.beta0, df = n-params, lower.tail = FALSE) 
p.beta1 <- 2 * pt(t.beta1, df = n-params, lower.tail = FALSE) 
p.beta2 <- 2 * pt(t.beta2, df = n-params) # because t = neg
p.beta3 <- 2 * pt(t.beta3, df = n-params, lower.tail = FALSE) 
p.beta4 <- 2 * pt(t.beta4, df = n-params) # because t = neg

# OR could take absolute value for every t
# so it always calculate the right-side tail

p.beta0 <- 2 * pt(abs(t.beta0), df = n-params, lower.tail = FALSE) 
p.beta1 <- 2 * pt(abs(t.beta1), df = n-params, lower.tail = FALSE) 
p.beta2 <- 2 * pt(abs(t.beta2), df = n-params, lower.tail = FALSE)
p.beta3 <- 2 * pt(abs(t.beta3), df = n-params, lower.tail = FALSE) 
p.beta4 <- 2 * pt(abs(t.beta4), df = n-params, lower.tail = FALSE)

# compare against alpha

if (p.beta0 < alpha) { print("Reject H0")} else { print("Do not reject H0") }
if (p.beta1 < alpha) { print("Reject H0")} else { print("Do not reject H0") }
if (p.beta2 < alpha) { print("Reject H0")} else { print("Do not reject H0") }
if (p.beta3 < alpha) { print("Reject H0")} else { print("Do not reject H0") }
if (p.beta4 < alpha) { print("Reject H0")} else { print("Do not reject H0") }

### ANSWER EXERCISE QUESTIONS ###

# Q. Are the variables significant?
# A. The intercept, height and age are potentially *not* significant, because
#    their p-values are high. However, APM and MPG are likely significant,
#    because their p-values are low.

# Q. Can a player's PPM be increased by increasing MPG?
# A. Not necessarily. A one-unit increase in MPG is associated with a 0.0077781
#    increase in PPM when all other variables are held constant. But in 
#    practice, giving a player more minutes on court won't necessarily improve
#    their PPM, which is already scaled for minutes on court. The 0.0077781 
#    increase might be because good players get more time, not the other way
#    around.

### PLOT STANDARDISED RESIDUALS VS FITTED VALUES ###

resid <- lm.fit$residuals
sigma.hat.sq <- sum(resid^2)/lm.fit$df.residual
resid.std <- resid / sqrt(sigma.hat.sq * (1-hat(X)))

plot(lm.fit$fitted.values, resid.std)
abline(h = c(-2, 0, 2), lty = c(2, 1, 2))

# interpretation:
# no clear pattern - good
# only four residuals that are >2 std deviations from mean


detach(bb)
