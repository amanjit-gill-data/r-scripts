# 4.5 RIDGE REGRESSION
# activity using hald dataset

library(glmnet)

hald <- read.table("hald.txt")
hald

# 1 identify multicollinearity

pairs(hald[,-1])
cor(hald[,-1])

# there may be relationships between x1 and x3, and x2 and x4
# confirmed by examining correlation matrix
# therefore possible collinearity but not multicollinearity

# 2

# standardise y
y <- (hald$y - mean(hald$y)) / sqrt(var(hald$y))

# now standardise x
X <- as.matrix(hald[, -1])

n <- length(y)
x.means <- apply(X, MARGIN = 2, FUN = mean)
x.vars <- apply(X, MARGIN = 2, FUN = var)
x.sds <- sqrt(x.vars)

X1 <- X - matrix(rep(x.means, times=n), nrow=n, byrow = TRUE) # subtract column mean from every value
X2 <- X1 %*% diag(1/x.sds) # X2 is the standardised X

# augment both X2 and y

lambda <- 0.02

X.aug <- rbind(X2, sqrt(lambda)*diag(4))
y.aug <- c(y, rep(0, times=4))

# fit a linear model

m <- lm(y.aug ~ X.aug - 1)
m$coefficients

# now fit a ridge regression and compare coefficients to the lm coeffs

rr <- glmnet(X2, y, alpha=0, lambda = 0.02, standardize = FALSE) # already standardised

rr$beta
rr$a0

# result: similar; not inconsistent with each other


