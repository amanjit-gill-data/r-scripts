# 6.1 GAM - COMPARE WITH FORWARD SELECTION
# using College dataset

library(ISLR) # for College data
library(leaps) # for regsubsets()
library(gam)

data("College")
attach(College)

n <- nrow(College)
n.pred <- ncol(College) - 1

# FIT WITH FORWARD SELECTION - WITH TRAIN/TEST SETS

# first split the data
set.seed(1)
indices <- sample(1:n, size=round(n/2, 0))
train.set <- College[indices, ]
test.set <- College[-indices, ]

# design matrix using test set, inc column of 1s; also converts Yes/No to 1/0
X <- model.matrix(Outstate ~ ., data=test.set)

# return the best model for every number of predictors
fit.fs <- regsubsets(Outstate ~ ., data=train.set, method="forward", nvmax=n.pred)

MSEs <- vector(length=n.pred)

# compute error for the best model for every number of predictors
for (i in 1:n.pred) {
  
  # vector of coefficients for best model of this size 
  coefs <- coef(fit.fs, id = i)
  
  # predictions: multiply design matrix (just the chosen variables) by coefs
  preds <- X[, names(coefs)] %*% coefs
  
  # MSE for best model of this size
  MSEs[i] <- mean((test.set$Outstate - preds)^2)
}

plot(1:n.pred, MSEs, main="MSE For Best Linear Model At Every Size", type="b")
which.min(MSEs) # 13

# FIT WITH FORWARD SELECTION - WITH TRAIN/VALIDATION/TEST SETS LIKE IN SOLUTION

# first split the data
set.seed(0)
set_allocation <- sample(1:3, n, replace = TRUE, prob = c(0.5, .25, .25))
train.set <- College[which(set_allocation == 1), ]
validation.set <- College[which(set_allocation == 2), ]
test.set <- College[which(set_allocation == 3), ]

# design matrix using validation set
X <- model.matrix(Outstate ~ ., data = validation.set)

# return the best model for every number of predictors
fit.fs <- regsubsets(Outstate ~ ., data=train.set, method="forward", nvmax=n.pred)

MSEs <- vector(length=n.pred)

# compute error for the best model for every number of predictors
# this time, use validation set rather than test set
for (i in 1:n.pred) {
  
  # vector of coefficients for best model of this size 
  coefs <- coef(fit.fs, id = i)
  
  # predictions: multiply design matrix (just the chosen variables) by coefs
  preds <- X[, names(coefs)] %*% coefs
  
  # MSE for best model of this size
  MSEs[i] <- mean((validation.set$Outstate - preds)^2)
}

plot(1:n.pred, MSEs, main="MSE For Best Linear Model At Every Size", type="b")
which.min(MSEs) # 9

# now use test set to confirm that 9 predictors yields a low MSE

test.matrix <- model.matrix(Outstate ~ ., data=test.set)
coefs <- coef(fit.fs, 9)
preds <- test.matrix[, names(coefs)] %*% coefs
(MSE <- mean((test.set$Outstate - preds)^2))

# FIT WITH GAM, USING THE SAME VARIABLES THAT WERE CHOSEN FOR THE BEST 
# 3-PREDICTOR MODEL IN FORWARD SELECTION

coef(fit.fs, 3) # Private, Room.Board, Expend

# rework the training, validation and test sets into just a training and 
# validation set, like in the solution
train.set <- College[which(set_allocation == 1 | set_allocation == 2), ]
test.set <- College[which(set_allocation == 3), ]

# fit GAM using training set
fit.gam <- gam(Outstate ~ s(Room.Board) + s(Expend) + Private, data=train.set)

par(mfrow = c(1, 3))
plot.Gam(fit.gam, se=TRUE, col="blue")

# TRY THE GAM ON THE TEST SET

preds <- predict(fit.gam, newdata = test.set)
(MSE <- mean((test.set$Outstate - preds)^2))

# observations
# MSE for GAM with the 3 predictors = 4570160
# MSE for Forward Selection Model with 3 predictors = 5097180
# therefore GAM has improved performance on the 3-predictor model

# ANOVA TO IDENTIFY NON-LINEAR AND LINEAR COMPONENTS

summary(fit.gam)

# results:
# both Room.Board and Expend have significant nonparametric/nonlinear effects
# Private is strictly linear
# therefore final formula is Outstate ~ s(Room.Board) + s(Expend) + Private






