library(dobson)
data("poisson")
attach(poisson)

head(poisson)

### VISUALISE DATASET ### 

# replace x-axis with a new axis that only has the possible numbers for x

plot(x, y, pch=16, xaxt = "n", ylim = c(0,15))
axis(1, at = c(-1, 0, 1))

### ESTIMATE PARAMETERS WITH MATHS (ITERATION) ###

# iters = int; no. iterations to do
# b = vector; an initial guess for b1 and b2
# x, y = vectors; observations
estimate.b <- function(iters, b, x, y) {
  
  # no. cases
  N <- length(y)
  
  # first set up the matrices
  
  y <- z <- as.matrix(y)
  
  # design matrix; initially a column of 1s, with an empty column next to it
  X <- matrix(c(rep(1, N), x), nrow = N, ncol = 2) 
  
  # where each progressive estimate for the parameters will go
  b.est <- matrix(nrow = iters, ncol = 2)
  
  # this is the iteration
  
  for (i in 1:iters) {
    
    w <- 1 / (b[1] + b[2]*x) # compute w with current estimates of b1 and b2
    W <- diag(w, N, N) # create W matrix with w repeating down the diagonal
    b <- b.est[i, ] <- solve(t(X) %*% W %*% X) %*% t(X) %*% W %*% z # new estimates
  }
  
  I.inv <- solve(t(X) %*% W %*% X)
  
  return(list(b.est = b.est, I.inv = I.inv, sd = sqrt(diag(I.inv))))
}

b.hat <- estimate.b(iters = 5, b = c(7,5), poisson$x, poisson$y)

### CONFIDENCE INTERVAL FOR EACH PARAMETER ###
# using b1 and b2 estimates from anywhere

alpha <- 0.05
lower <- tail(b.hat$b.est, 1) - qnorm(1 - alpha/2)*b.hat$sd
upper <- tail(b.hat$b.est, 1) + qnorm(1 - alpha/2)*b.hat$sd

lower
upper

### ESTIMATE PARAMETERS WITH GLM OBJECT - IDENTITY LINK ###

m.id <- glm(y ~ x, family = poisson(link = "identity"), data = poisson)

m.id$coefficients # matches iteration

### ESTIMATE PARAMETERS WITH GLM OBJECT - CANONICAL LINK ###

# link="log" (i.e. canonical) isn't needed; it's the default
m.can <- glm(y ~ x, family = poisson(link = "log"), data = poisson)

m.can$coefficients # different to iteration and identity link but still significant

### ASSESS PARAMETER ESTIMATES WITH RATE RATIO ###

(RR <- exp(m.can$coefficients[2]))

### ASSESS MODEL FIT USING DIFFERENT STATISTICS ###

# pearson residuals from glm object
residuals.glm(m.can, type = "pearson")

# pearson residuals from formula with observed and expected responses
(p.resid <- c(y - m.can$fitted.values) / sqrt(m.can$fitted.values))

# pearson chi-squared statistic using p.resid from anywhere
(psq <- sum(p.resid**2))

# deviance from glm object
m.can$deviance

# deviance from deviance residuals from glm object
d.resid <- residuals.glm(m.can, type = "deviance")
(D <- sum(d.resid**2))

# critical chi-squared value
alpha <- 0.05
N <- nrow(poisson) # sample size
p <- length(m.can$coefficients) # parameters
crit <- qchisq(alpha, df=N-p, lower.tail = FALSE)

# hypothesis test with both P^2 and D
# H0: model is a GOOD fit
# result: do not reject H0; i.e. this is a good fit
cat("P^2 Test: Reject = ", psq > crit)
cat("D Test: Reject = ", D > crit)

### COMPARE TO NULL MODEL USING DEVIANCES ###

# method 1: take M0 and M1 deviances from glm object
d0 <- m.can$null.deviance
d1 <- m.can$deviance
(DeltaD <- d0 - d1)

# method 2: create separate model objects and take deviances from them
# canonical link function, by default
m0 <- glm(y ~ 1, family = poisson(), data = poisson)
m1 <- glm(y ~ x, family = poisson(), data = poisson)
(DeltaD <- m0$deviance - m1$deviance)

# method 3: create separate model objects and calculate DeltaD from log-likelihoods
# this gives C, the log-likelihood ratio statistic, if M0 is the null model
# use same M0 and M1 as defined above
(DeltaD <- 2 * (logLik(m1) - logLik(m0)))

# critical chi-squared value
alpha <- 0.05
p <- length(m1$coefficients) # parameters in M1
q <- length(m0$coefficients) # parameters in M0
crit <- qchisq(alpha, df=p-q, lower.tail = FALSE)

# hypothesis test with DeltaD
# H0: complex model M1 isn't better than M0 i.e. b2 = 0
# result: reject H0; M1 is a better fit than the null model M0
cat("P^2 Test: Reject = ", DeltaD > crit)

### COMPARE TO NULL MODEL USING PSEUDO-R^2 ###

# pseudo-R^2 using glm objects in log-likelihood formula
# start with separate model objects for M0 and M1

(pseudo.Rsq <- (logLik(m0) - logLik(m1)) / logLik(m0))

# result (from Ed notes): small, but this is *not* alarming, as pseudo-R^2 is
# a measure of the predictability of the individual outcome Yi, not of all
# the event rates