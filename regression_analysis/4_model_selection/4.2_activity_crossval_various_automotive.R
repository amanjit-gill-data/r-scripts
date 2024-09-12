# 4.2 M-FOLD CROSS VALIDATION
# Activity on Auto dataset

library(ISLR)
data("Auto")
head(Auto)

N <- nrow(Auto)
deg <- 7
nrep <- 10

# 1
# fit polynomials up to order 7
# use evenly split training and testing test

mse <- matrix(nrow=nrep, ncol=deg) # 1 row per repetition, 1 col per model
  
for (i in 1:nrep) {
  
    set.seed(i)
    train.set.indices <- sample(x=1:N, size=N/2, replace=FALSE)
    train.set <- Auto[-train.set.indices, ]
    test.set <- Auto[train.set.indices, ]
    
    for (j in 1:deg) {
      m <- lm(mpg ~ poly(horsepower, j), data=train.set)
      pred.vals <- predict(m, test.set)
      mse[i,j] <- mean((test.set$mpg - pred.vals)^2)
    }
}

plot(NULL, xlab="Deg of Polynomial", ylab="MSE", xlim=c(1,7), ylim=c(15,30))
title("Evenly split train/test split; 10 repetitions")

for (i in 1:nrep) {
  points(mse[i,], pch=16, col=i, type="b")
}

# 2 
# LOOCV curve wrt polynomial order
# no need for repetitions, as this is virtually bias-free

mse <- vector(length = deg)

for (i in 1:deg) {
  
  m <- lm(mpg ~ poly(horsepower, i), data=Auto)
  mse[i] <- mean((m$residuals / (1 - hatvalues(m)))**2)
}

plot(NULL, xlab="Deg of Polynomial", ylab="MSE", xlim=c(1,7), ylim=c(15,30))
title("LOO-CV")

points(x=1:7, y=mse, pch=16, type="b")

# 3
# 10-fold CV with 10 repetitions; plot the 10 curves

folds <- 10 # no. folds

mse <- matrix(nrow=nrep, ncol=deg) # 1 row per repetition, 1 col per model

for (i in 1:nrep) {
  
  set.seed(i)
  shuffle.ids <- sample(1:N, size=N, replace=FALSE)
  Auto.shuffled <- Auto[shuffle.ids, ]
  
  seq.ids <- round(seq(0,N,by=N/folds),0)
  
  for (j in 1:deg) {
    
    mse.fold <- vector(length=folds)
    
    for (k in 1:folds) {
      
      test.ids <- (seq.ids[k]+1):seq.ids[k+1] # e.g. 1st test set = indices 1-39
      test.set <-Auto.shuffled[test.ids, ]
      train.set <-Auto.shuffled[-test.ids, ]
      
      m <- lm(mpg ~ poly(horsepower, j), data=train.set)
      pred.vals <- predict(m, test.set)
      
      mse.fold[k] <- mean((test.set$mpg - pred.vals)^2)
    }
    
    mse[i, j] <- mean(mse.fold)
  }
}

plot(NULL, xlab="Deg of Polynomial", ylab="MSE", xlim=c(1,7), ylim=c(18,25))
title("10-fold CV; 10 repetitions")

for (i in 1:nrep) {
  points(mse[i,], pch=16, col=i, type="b")
}

# 4
# observations

# fewer folds -> more variability in the error 
# more folds -> less variability in the error
# both show error around 20, for polynomial degrees 2-7
# therefore for parsimony, choose degree 2





