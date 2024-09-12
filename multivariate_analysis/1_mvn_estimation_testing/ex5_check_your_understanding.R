# WEEK 1: CONFIDENCE INTERVALS AND HYPOTHESIS TESTING FOR MEAN VECTOR

# Q2

n <- 50
sigma <- rbind(c(3,1,1), c(1,4,1), c(1,1,2))
xbar <- (c(.8, 1.1, .6))
mu0 <- (c(0,0,0))
p <- 3
alpha <- 0.05

u_squared <- n * t(xbar - mu0) %*% solve(sigma) %*% (xbar - mu0)
u_squared

crit_chisq <- qchisq(1-alpha, 3)
crit_chisq

# reject if u_squared > crit_chisq
# result: reject h0

# Q3

mu_0 <- c(7,11)
X <- rbind(c(2,8,6,8), c(12,9,9,10))
xbar <- c(6,10)
p <- 2
n <- 4
alpha <- 0.05
S <- rbind(c(24,-10), c(-10,6))/3

T_sq <- n * t(xbar - mu_0) %*% solve(S) %*% (xbar - mu_0)
F <- T_sq/3
F

crit_val <- qf(1-.05, 2, 2)
crit_val

