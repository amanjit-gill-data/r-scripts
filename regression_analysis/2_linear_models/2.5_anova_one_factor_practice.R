# 2.5 ANOVA - Practice

plant <- read.table("plant.txt", header = TRUE)
attach(plant)
head(plant)
n <- nrow(plant)

y <- weight

### MODEL 1 ###

# design matrix
# all 1s and 0s
# 1st column = 10 1s, 20 0s
# 2nd column = 10 0s, 10 1s, 10 0s
# 3rd column = 20 0s, 10 1s

X1 <- cbind ( 
  c(rep(1,10), rep(0,20)),
  c(rep(0,10), rep(1,10), rep(0,10)),
  c(rep(0,20),rep(1,10))
)

# b.hat is just the usual formula for b.hat
# each element (one for each group) is the mean for each group
(b.hat <- solve(t(X1) %*% X1) %*% t(X1) %*% y)

### MODEL 2 ###

# design matrix is same as for Model 1, but with an additional column of 1s
X2 <- cbind(rep(1, n), X1)

# both lambda and mu.hat = grand mean of response
lambda <- mu.hat <- mean(y)

# treatment effect alpha - using aggregate() function for grouped means
alpha.hat <- aggregate(weight, list(group), mean)$x - lambda

# treatment effect alpha - using by() function for grouped means
alpha.hat <- by(weight, group, FUN = mean) - lambda

### MODEL 3 ###

# design matrix is a column of 1s, and columns for groups 2 & 3; not for group 1
X3 <- cbind(
  rep(1, n),
  c(rep(0,10), rep(1,10), rep(0,10)),
  c(rep(0,20), rep(1,10))
)

# b.hat is just the usual formula for b.hat
# elements are the group 1 mean, and treatment effect for groups 2 & 3 
(b.hat <- solve(t(X3) %*% X3) %*% t(X3) %*% y)

### HYPOTHESIS TESTING - USING MATH ###

# uses the usual b.hat and deviance formulas
# no need to compute sigma.hat.sq for deviance formula, as this will cancel 
# out in f.stat calculation

# null model
# design matrix is just a column of 1s
# b.hat is 1x1, equals the grand mean of response
X0 <- cbind(rep(1, n))
(b.hat0 <- solve(t(X0) %*% X0) %*% t(X0) %*% y)
D0 <- t(y) %*% y - t(b.hat0) %*% t(X0) %*% y 

# model of interest (just use Model 3)
D1 <- t(y) %*% y - t(b.hat) %*% t(X3) %*% y 

# method 1: compare f-statistic to critical value

J <- 3
alpha <- 0.05

f.stat <- (D0 - D1)/(J - 1) / (D1 / (n - J))
f.crit <- qf(alpha, J-1, n-J, lower.tail = FALSE)
if (f.stat > f.crit) { print("Reject H0") } else { print("Do not reject H0") }

# method 2: compare p-value to alpha

p.value <- pf(f.stat, J-1, n-J, lower.tail = FALSE)
if (p.value < alpha) { print("Reject H0") } else { print("Do not reject H0") }

### HYPOTHESIS TESTING - USING LM OBJECT ###

# shows p.value = 0.01591, which is less than 0.05 --> reject H0
# also shows f.stat = 4.846, which can be compared to f.crit 
lm.fit <- lm(weight ~ group)
summary(lm.fit)


