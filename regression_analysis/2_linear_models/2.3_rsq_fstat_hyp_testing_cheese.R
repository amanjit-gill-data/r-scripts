# 2.3 R^2, F-Statistic, Hypothesis Testing - Exercises on Cheese Data

cheese <- read.table("cheese.dat", header=TRUE)
attach(cheese)
head(cheese)
n <- nrow(cheese)

### R^2 - USING MATHS ###

X <-  cbind(rep(1, n), acetic, H2S, lactic)
y <- taste
ybar <- mean(y)
b.hat <- solve(t(X) %*% X) %*% t(X) %*% y

RSS <- t(y) %*% y - t(b.hat) %*% t(X) %*% y
TSS <- sum((y - ybar)^2)

(R.sq <- (TSS - RSS) / TSS)

# R^2 - USING LM OBJECT ###

lm.fit <- lm(taste ~ acetic+H2S+lactic)
summary(lm.fit)$r.squared

### F-STATISTIC - USING R^2 ###

p <- 4
(f.stat <- R.sq/(1-R.sq) * (n-p)/(p-1))

### F-STATISTIC - USING LM OBJECT ###

summary(lm.fit)$fstatistic[1]

### F-STATISTIC - USING DEVIANCE FROM GLM OBJECT ###

glm.fit <- glm(taste ~ acetic+H2S+lactic, family = gaussian)
D0 <- glm.fit$null.deviance
D1 <- glm.fit$deviance

p <- 4
q <- 1
(f.stat <- (D0 - D1)/D1 *(n-p)/(p-q))

### F-STATISTIC - USING DEVIANCE FROM MATH ###

# deviance (x sigma.hat.sq) of model of interest
D1 <- t(y) %*% y - t(b.hat) %*% t(X) %*% y 

# deviance (x sigma.hat.sq) of null model
X0 <- cbind(rep(1, n))
(b.hat0 <- solve(t(X0) %*% X0) %*% t(X0) %*% y)
D0 <- t(y) %*% y - t(b.hat0) %*% t(X0) %*% y 

(f.stat <- (D0 - D1)/D1 *(n-p)/(p-q))

### HYPOTHESIS TEST ###

# method 1 - compare f-statistic

alpha <- 0.05
f.crit <- qf(alpha, p-1, n-p, lower.tail = FALSE)

if (f.stat > f.crit) { print("Reject H0") } else { print("Do not reject H0") }

# method 2 - compare p-value

p.value <- pf(f.stat, p-1, n-p, lower.tail = FALSE)

if (p.value < alpha) { print("Reject H0") } else { print("Do not reject H0") }



