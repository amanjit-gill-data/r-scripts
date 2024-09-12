# 4.5 RIDGE REGRESSION
# demo on ridge regression with glmnet(), using manpower dataset
# must set alpha = 0 to use ridge regression

library(bestglm)
library(glmnet)
data("manpower")
head(manpower)

# FIT WITH GLMNET()

x <- as.matrix(manpower[, 1:4])
y <- log(manpower$Hours) # log transform due to skew
lambdas <- exp(seq(-6, 6, 0.2)) # try lots of shrinkage parameters

fit <- glmnet(x, y, alpha = 0, lambda = lambdas)

# plot shrinking of each coefficient wrt log-lambda
plot(fit, xvar = "lambda", label = TRUE)

# FIT WITH GLMNET() AND M-FOLD CROSS VALIDATION

set.seed(1)
cv.fit <- cv.glmnet(x, y, alpha = 0, nfolds = 5, lambda = lambdas)
plot(cv.fit)

# find minimum lambda
min.lambda <- cv.fit$lambda.min # min lambda is 1

# find largest lambda 1 SD away from the min lambda
chosen.lambda <- cv.fit$lambda.1se

# find coefficients if conservative lambda is chosen
fit$beta[, cv.fit$index[,1]]

# find coefficients if min lambda is chosen
fit$a0[cv.fit$index[,1]]

