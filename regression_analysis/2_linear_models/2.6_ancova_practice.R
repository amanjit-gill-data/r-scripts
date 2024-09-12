# 2.6 ANCOVA - Practice

ach <- read.table("achievement.txt", header = TRUE)
attach(ach)
head(ach)
n <- nrow(ach)

method <- as.factor(method)

# 3 x 3 plot - each pair of variables
plot(ach)

# achievement score vs initial aptitude
plot(x, y, col=method, pch=16, xlab="aptitude", ylab="achievement")
legend(x="bottomright", legend = c("A", "B", "C"), col=c(1,2,3), 
       pch=16, y.intersp = 0.7, bg="transparent", bty="n")

### FIT MODEL WITH LM/GLM ###

lm.fit <- lm(y ~ x+method)
summary(lm.fit)

glm.fit <- glm(y ~ x+method, family = gaussian)
summary(glm.fit)

### FIT MODEL WITH MATHS ###

# null model
# design matrix is a column of 1s
# b.hat0 is 1x1, equals the grand mean of response
X0 <- cbind(rep(1, n))
(b.hat0 <- solve(t(X0) %*% X0) %*% t(X0) %*% y)
D0 <- t(y) %*% y - t(b.hat0) %*% t(X0) %*% y 

# model of interest
# design matrix:
# col1 = 1s, 0s,
# col2 = 0s, 1s, 0s
# col3 = 0s, 1s
# col4 = all x-values for all levels
X <- cbind ( 
  c(rep(1,7), rep(0,14)),
  c(rep(0,7), rep(1,7), rep(0,7)),
  c(rep(0,14),rep(1,7)),
  x
)
(b.hat <- solve(t(X) %*% X) %*% t(X) %*% y)
D1 <- t(y) %*% y - t(b.hat) %*% t(X) %*% y 

# hypothesis test

p <- 4 #estimates
n <- 21
f.stat <- (D0 - D1)/(p-1) / (D1 / (n-p))
f.crit <- qf(0.05, p-1, n-p, lower.tail = FALSE)
# reject H0

p.value <- pf(f.stat, p-1, n-p, lower.tail = FALSE)
# reject H0


### REDUCED MODEL (EXAMPLE IN DOBSON & BARNETT) ###

# design matrix is a column of 1s, plus all x
# b.hat0 is 2x1
X0 <- cbind(rep(1, n), x)
(b.hat0 <- solve(t(X0) %*% X0) %*% t(X0) %*% y)
D0 <- t(y) %*% y - t(b.hat0) %*% t(X0) %*% y 

# hypothesis test

p <- 4 #estimates for model of interest
q <- 2 #estimates for reduced model
n <- 21
f.stat <- (D0 - D1)/(p-q) / (D1 / (n-p))
f.crit <- qf(0.05, p-q, n-p, lower.tail = FALSE)
# reject H0

p.value <- pf(f.stat, p-q, n-p, lower.tail = FALSE)
# reject H0


