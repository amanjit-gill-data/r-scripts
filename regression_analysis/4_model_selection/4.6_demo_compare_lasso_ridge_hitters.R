# 4.6 LASSO VS RIDGE REGRESSION

library(ISLR)

# load and clean dataset in one step
Hitters=na.omit(Hitters)

# PREPARE DATASET

# convert A/N and E/W columns to 0/1
# uses design matrix method which creates a column of 1s, which we don't
# need, so we remove it immediately
x=model.matrix(Salary~., Hitters)[,-1] 

y=Hitters$Salary

# USE CV.GLMNET() TO CHOOSE LAMBDA

# create 1 train and 1 test set

lambda=exp(seq(-8.1,1,0.1))

set.seed(1)
train=sample(1:nrow(x),nrow(x)/2, replace = FALSE)
test=(-train)
y.test=y[test]

# run cv.glmnet() with cross validation
# there are no folds, so no need to specify nfolds

set.seed(1)
cv.out=cv.glmnet(x[train, ], y[train], alpha=0)
best.lambda=cv.out$lambda.min

# find the MSE when lambda is the chosen value
# first fit the real model (but not using the chosen lambda)
ridge.m <- glmnet(x[train, ], y[train], alpha=0)
pred.vals <- predict(ridge.m, s = best.lambda, newx = x[test, ])
mse <- mean((y.test - pred.vals)^2)
mse

# refit using full dataset, then print coeffs for chosen lambda
out=glmnet(x,y,alpha=0)
predict(out, type="coefficients",s=best.lambda)[1:20,]

# LASSO

# same model as before, but with alpha = 1

# first find the best lambda using cv.glmnet
set.seed(1)
cv.out=cv.glmnet(x[train, ], y[train], alpha=1) # just changed the alpha
best.lambda=cv.out$lambda.min

# find the MSE when lambda is the chosen value
# first fit the real model (but not using the chosen lambda)
lasso.mod=glmnet(x[train,], y[train], alpha=1)
lasso.pred=predict(lasso.mod, s=best.lambda, newx=x[test,])
mean((lasso.pred-y.test)^2)

# refit using full dataset, then print coeffs for chosen lambda
out=glmnet(x,y,alpha=1)
predict(out, type="coefficients",s=best.lambda)[1:20,] 


