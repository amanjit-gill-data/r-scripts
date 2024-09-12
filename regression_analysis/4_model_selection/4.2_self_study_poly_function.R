# TEST raw=TRUE and raw=FALSE in poly() function

### general comparison of raw=TRUE/FALSE ###

X <- c(.1,.2,.3,.4,.5,.6,.7,.8,.9,.10)
Y <- c(.1,.3,.5,.7,.9,.11,.13,.15,.17,.19)

m1 <- lm(Y ~ poly(X, 3, raw=FALSE))
m2 <- lm(Y ~ poly(X, 3, raw=TRUE))

all.equal(m1$fitted.values, m2$fitted.values)
all.equal(m1$residuals, m2$residuals)
all.equal(hatvalues(m1), hatvalues(m2))

# the coefficients are different but result in the same predictions
predict(m1, newdata = data.frame(X=c(.25, .75)))
predict(m2, newdata = data.frame(X=c(.25, .75)))


### comparing design matrices created with poly() and model.matrix() ###
### using generated data from quiz 4                                 ###

library(leaps)

X_matrix1 <- cbind(X, X^2, X^3, X^4, X^5)
X_matrix2 <- model.matrix(Y ~ poly(X, 5, raw=TRUE))[, -1] # same as X_matrix1
X_matrix3 <- model.matrix(Y ~ poly(X, 5, raw=FALSE))[, -1] # different to X_matrix1

head(X_matrix1)
head(X_matrix2)
head(X_matrix3)

set.seed(1)

# use X matrix I made myself by computing the powers of X
bs1 <- regsubsets(Y ~ ., data=data.frame(Y, X_matrix1), nvmax = 10)

# use X matrix I made myself with poly(raw=TRUE)
bs2 <- regsubsets(Y ~ ., data=data.frame(Y, X_matrix2), nvmax = 10)

# supply poly(raw=TRUE) to regsubsets() so it computes the powers on its own
bs3 <- regsubsets(Y ~ poly(X, 5, raw=TRUE), data=data.frame(Y, X), nvmax = 10)

coef(bs1, 3)
coef(bs2, 3)
coef(bs3, 3)

summary(bs1)


