# 4.5 RIDGE REGRESSION
# big activity using credit dataset

library(ISLR)
library(glmnet)
data("Credit")

Credit <- Credit[,-1] # remove ID column
head(Credit)

# 1 define y and X
y <- Credit$Balance
X <- model.matrix(Balance ~ ., data=Credit)[, -1] # don't need the column of 1s

# 2 grid for lambda, then do ridge regression, then plot coeffs wrt lambda
lambdas <- 10^(seq(5, -2, length=100))

rr <- glmnet(X, y, alpha=0, lambda=lambdas)

plot(
  seq(5, -2, length=100), rr$beta["Income", ], type="l",
  xlab="log-lambda / log10", ylab="coeff"
)

# 3 compare ordinary regression with ridge regression when lambda = small

lm <- lm(y ~ X)

cbind(lm$coefficients, coef(rr)[,100])

# 4 compare ordinary regression with ridge regression when lambda = large

cbind(lm$coefficients, coef(rr)[,1])

# 5 split into train and test sets; then compare lm to rr

set.seed(1)
train.ids = sample(1:nrow(X),nrow(X)/2, replace = FALSE)
test.ids=(-train.ids)

# ordinary linear model
lm <- lm(y[train.ids] ~ X[train.ids, ])
lm.preds <- coef(lm)[1] + X[test.ids, ] %*% coef(lm)[-1]
mean((y[test.ids] - lm.preds)^2)

# ridge regression with 3 lambdas
rr <- glmnet(X[train.ids, ], y[train.ids], alpha=0, lambda=c(0.01, 7, 20), thresh = 1e-12)

rr.preds1 <- predict(rr, s = 0.01, newx = X[test.ids, ])
mean((y[test.ids] - rr.preds1)^2)

rr.preds2 <- predict(rr, s = 7, newx = X[test.ids, ])
mean((y[test.ids] - rr.preds2)^2)

rr.preds3 <- predict(rr, s = 20, newx = X[test.ids, ])
mean((y[test.ids] - rr.preds3)^2)

# 6 use cv.glmnet to choose lambda, then use the actual model to find its MSE

set.seed(2)

rr.cv <- cv.glmnet(X[train.ids, ], y[train.ids], alpha=0, nfolds=5)
plot(rr.cv)

min.lambda <- rr.cv$lambda.min

rr <- glmnet(X[train.ids, ], y[train.ids], alpha=0, lambda=lambdas, thresh = 1e-12)
rr.preds <- predict(rr, s = min.lambda, newx = X[test.ids, ])
mean((y[test.ids] - rr.preds)^2)

#7 use cv.glmnet again, but this time use the lambdas we specified

set.seed(2)

rr.cv <- cv.glmnet(X[train.ids, ], y[train.ids], lambda=lambdas, alpha=0, nfolds=5)
plot(rr.cv)

min.lambda <- rr.cv$lambda.min

rr <- glmnet(X[train.ids, ], y[train.ids], alpha=0, lambda=lambdas, thresh = 1e-12)
rr.preds <- predict(rr, s = min.lambda, newx = X[test.ids, ])
mean((y[test.ids] - rr.preds)^2)


