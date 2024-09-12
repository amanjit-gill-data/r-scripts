# 1.3 Australian longitudinal study on women's health

### DATA ###

# town
Y1 <- c(0,1,1,0,2,3,0,1,1,1,1,2,0,1,3,0,1,2,1,3,3,4,1,3,2,0)
K1 <- length(Y1)

# country
Y2 <- c(2,0,3,0,0,1,1,1,1,0,0,2,2,0,1,2,0,0,1,1,1,0,2)
K2 <- length(Y2)

N <- K1 + K2

### FUNCTION DEFS ###

# evaluate expression for each element in Y_vect, then sum them all up
# Y_vect can be one vector, or a vector of vectors i.e. multiple datasets
log_summation <- function(Y_vect, theta) {
  sum(Y_vect * log(theta) - theta - log(factorial(Y_vect)))
}

# sum elements of Y_vect, then divide by N
# Y_vect can be one vector, or a vector of vectors i.e. multiple datasets
theta_summation <- function(Y_vect, N) {
  sum(Y_vect) / N
}

# return vector of residuals for one dataset
residuals <- function(Y, theta_hat, round_to) {
  round((Y - theta_hat) / sqrt(theta_hat), round_to)
}

### EXERCISES ###

# compute parameter estimate and log-likelihood under H0
theta_hat_h0 <- theta_summation(c(Y1, Y2), N)
logl_h0 <- log_summation(c(Y1, Y2), theta_hat_h0)

# compute parameter estimates and log-likelihood under H1
theta_hat1_h1 <- theta_summation(Y1, K1)
theta_hat2_h1 <- theta_summation(Y2, K2)
logl_h1 <- log_summation(Y1, theta_hat1_h1) + log_summation(Y2, theta_hat2_h1)

# plot residuals under H0
resid1_h0 <- residuals(Y1, theta_hat_h0, 2)
resid2_h0 <- residuals(Y2, theta_hat_h0, 2)
plot(table(resid1_h0))
plot(table(resid2_h0))

# plot residuals under H1
resid1_h1 <- residuals(Y1, theta_hat1_h1, 2)
resid2_h1 <- residuals(Y2, theta_hat2_h1, 2)
plot(table(resid1_h1))
plot(table(resid2_h1))

# compute test statistic under H0
sum_rsq_h0 <- sum(c(resid1_h0, resid2_h0)**2)
m_h0 <- K1 + K2 - 1

# compute test statistic under H1
sum_rsq_h1 <- sum(c(resid1_h1, resid2_h1)**2)
m_h1 <- K1 + K2 - 2

# difference between test statistics
sum_rsq_h0 - sum_rsq_h1

