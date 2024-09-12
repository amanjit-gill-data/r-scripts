# 6.2 GAM - BACKFITTING FROM SCRATCH
# self-study, redo activity using my own code from the demo

# GENERATE DATA - SAME AS IN ACTIVITY

set.seed(0)

x1 <- rnorm(n=100)
x2 <- rnorm(n=100)
error <- 0.1*rnorm(100) # 0.1 to make it smaller than predictors

# true parameters

b0 <- 3
b1 <- 5
b2 <- -0.2

x <- cbind(x1, x2)
y <- b0 + b1*x1 + b2*x2 + error

# SET UP BACKFITTING

m <- 2 # no. variables (X1 and X2)
n <- 100 # no. cases
f.hat <- x*0
alpha <- mean(y)

rss.curr <- 0
rss.prev <- 10 # arbitrary; to ensure the script enters the loop at iteration 1

# ITERATE UNTIL CONVERGENCE




