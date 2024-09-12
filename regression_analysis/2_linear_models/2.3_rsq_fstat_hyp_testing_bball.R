# 2.3 R^2, F-Statistic, Hypothesis Testing - Exercises on Basketball Data

bb <- read.table("basketball.txt", header=TRUE)
attach(bb)
head(bb)
n <- nrow(bb)

### R^2 COEFF OF DETERMINATION ***

# with maths

X <-  cbind(rep(1, n), Height, APM, MPG, Age)
y <- PPM
ybar <- mean(y)
beta.hat <- solve(t(X) %*% X) %*% t(X) %*% y

RSS <- t(y) %*% y - t(beta.hat) %*% t(X) %*% y
TSS <- sum((y - ybar)^2)

(R.sq <- (TSS - RSS) / TSS)

# with lm model

lm.fit <- lm(PPM ~ Height + APM + MPG + Age)
summary(lm.fit)$r.squared

# interpretation
# 42% of the variability in the response can be explained by this model
# this indicates that the chosen variables may not be large contributors to
# the response, and other explanatory variables should be sought
# in addition, only significant variables should be included, but this model
# includes a bunch of insignificant ones (according to their p-values)

### F-STATISTIC AND P-VALUE - FROM LM OBJECT ###

f.stat <- summary(lm.fit)$fstatistic

# accompanying p-value is 3.664e-10
# can also be found with:
p.value <- pf(f.stat, 4, 91, lower.tail = FALSE)

### F-STATISTIC AND P-VALUE - USING DEVIANCE FROM MATHS ###

# null/minimal model
X0 <- cbind(rep(1,n))
q <- 1
p <- 5
beta.hat0 <- solve(t(X0) %*% X0) %*% t(X0) %*% y
y.hat0 <- X0 %*% beta.hat0

# deviance
D <- function(sigma.hat.sq, y, beta.hat, X) {
  (t(y) %*% y - t(beta.hat) %*% t(X) %*% y) / sigma.hat.sq 
}

# scaled deviance DeltaD
sigma.hat.sq <- RSS / (n - p)
DeltaD <- D(sigma.hat.sq, y, beta.hat0, X0) - D(sigma.hat.sq, y, beta.hat, X)

# F-statistic
f.stat <- DeltaD/(p-q) / (D(sigma.hat.sq, y, beta.hat, X)/(n-p))

# p-value
p.value <- pf(f.stat, p-1, n-p, lower.tail = FALSE)


### F-STATISTIC - USING DEVIANCE FROM GLM OBJECT

glm.fit <- glm(PPM ~ Height + APM + MPG + Age, family = gaussian)

D0 <- glm.fit$null.deviance
D1 <- glm.fit$deviance

(f.stat <- (D0 - D1)/(p-q) * (n-p)/D1)


### F-STATISTIC - USING R^2 OBTAINED BY ANY MEANS ###

R.sq <- summary(lm.fit)$r.squared

(f.stat <- R.sq/(1-R.sq) * (n-p)/(p-1))

### HYPOTHESIS TEST FOR OVERALL MODEL FIT ###

# method 1 - compare F-statistic

alpha <- 0.05
f.crit <- qf(alpha, p-1, n-p, lower.tail = FALSE)

if (f.stat > f.crit) { print("Reject H0") } else { print("Do not reject H0") }

# method 2 - compare p-value

p.value <- pf(f.stat, p-1, n-p, lower.tail = FALSE)

if (p.value < alpha) { print("Reject H0") } else { print("Do not reject H0") }

