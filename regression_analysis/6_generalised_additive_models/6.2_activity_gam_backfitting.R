# 6.2 GAM - BACKFITTING
# activity using generated data

# 1. GENERATE DATA

set.seed(0)

X1 <- rnorm(n=100)
X2 <- rnorm(n=100)
error <- 0.1*rnorm(100) # 0.1 to make it smaller than predictors

# true parameters

b0 <- 3
b1 <- 5
b2 <- -0.2

Y <- b0 + b1*X1 + b2*X2 + error

# 2. FIT THE MODEL Y - b1.hat*X1 = b0 + b2*X2 + error,
# KEEPING b1.hat FIXED AT ANY VALUE

b1.hat <- -3 # arbitrary; same as solution

fit1 <- lm(Y - b1.hat*X1 ~ X2) # recall we don't depict b0 and error in lm()


# 3. FIT THE MODEL Y - b2.hat*X2 = b0 + b1*X1 + error,
# KEEPING b2.hat FIXED AT THE ESTIMATE OBTAINED FROM PREVIOUS FIT

b2.hat <- coef(fit1)[2]

fit2 <- lm(Y - b2.hat*X2 ~ X1)

# 4. REPEAT STEPS 2 & 3, 10 TIMES; PLOT PARAMETER ESTIMATES FOR EACH ITERATION

n.reps <- 10
b0.hats <- b1.hats <- b2.hats <- vector(length=n.reps)
b1.hat <- -3 # initialise

for (i in 1:n.reps) {
  
  b2.hat <- lm(Y - b1.hat*X1 ~ X2)$coefficients[2]
  b1.hat <- lm(Y - b2.hat*X2 ~ X1)$coefficients[2]
  b0.hat <- lm(Y - b2.hat*X2 ~ X1)$coefficients[1]
  
  b0.hats[i] <- b0.hat
  b1.hats[i] <- b1.hat
  b2.hats[i] <- b2.hat
}

matplot(1:n.reps, cbind(b0.hats, b1.hats, b2.hats),
        main="Backfitting to Estimate Parameters",
        xlab="Iteration", ylab="Estimate",
        col=c("magenta", "blue", "orange"), pch=19, type="b", lwd=1.5)

abline(h=c(b0, b1, b2), col="darkgrey", lty="dashed") # show true values

legend(x=6, y=2.5, legend=c("b0.hat", "b1.hat", "b2.hat", "true value"),
       c("magenta", "blue", "orange", "darkgrey"),
       cex=0.75, bty="n")

# observations:
# final estimates are very close to true parameter values

# 5. FIT SAME PARAMETERS USING MULTIPLE LINEAR REGRESSION

mlr.fit <- lm(Y ~ X1 + X2) 

# add MLR estimates to plot
abline(h = mlr.fit$coefficients)

mlr.fit$coefficients

b0.hats[n.reps]
b1.hats[n.reps]
b2.hats[n.reps]

# observations:
# MLR estimates exactly match the final estimates from backfitting

