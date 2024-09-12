# 5.3 POLYNOMIALS - REGRESSION, SELECT DEGREE, SPLINE
# activity using Boston dataset

library(MASS) # just for Boston dataset
library(splines) # for bs()
data("Boston")
attach(Boston)

par(mfrow = c(1,1))

# 1. cubic polynomial fit; orthogonal (raw=FALSE)

pfit <- lm(nox ~ poly(dis, 3))
summary(pfit)

x <- seq(min(dis), max(dis), length.out = 100)
y <- predict(pfit, newdata = list(dis=x))

plot(dis, nox, cex=0.5, main="cubic polynomial")
lines(x, y, lwd=1.5, col="magenta")

# 2. fit different degrees and plot RSS

d <- 10 # highest degree to try
rss <- vector(length = d)

for (i in 1:d) {
  pfit <- lm(nox ~ poly(dis, i))
  rss[i] <- sum(pfit$residuals**2)
}

plot(x=1:d, y=rss, type="b", lwd=1.5, col="magenta", pch=16, main="RSS for Degrees 1 to 10")

# 3. use k-fold cross validation to select degree

folds <- 10
n <- nrow(Boston)
dmax <- 10 # highest degree to try

# first assign each case to a fold
# choose from 1 to folds, n times
# this method isn't the best because it can result in some folds being
# bigger than others
set.seed(1)
folds.assigned <- sample(x=1:folds, size=n, replace=TRUE)

# to store error for each degree
rss.train.deg <- vector(length=dmax)
rss.test.deg <- vector(length=dmax)

for (d in 1:dmax) {
  
  # to store error for each fold
  rss.train.folds <- vector(length=folds)
  rss.test.folds <- vector(length=folds)
  
  for (k in 1:folds) {
    
    # create train and test sets
    # folds.assigned != k returns a vector of TRUE/FALSE; use as a mask
    train.set <- Boston[folds.assigned != k, ] 
    test.set <- Boston[folds.assigned == k, ]
    
    # fit using train set
    fit <- lm(nox ~ poly(dis, d), data=train.set)
    
    # training error
    yhat.train <- predict(fit, newdata=train.set)
    rss.train.folds[k] <- sum((train.set$nox - yhat.train)^2)
    
    # testing error
    yhat.test <- predict(fit, newdata=test.set)
    rss.test.folds[k] <- sum((test.set$nox - yhat.test)^2)
  }
  
  # once the folds are done, compute overall error for the current degree
  
  rss.train.deg[d] <- mean(rss.train.folds)
  rss.test.deg[d] <- mean(rss.test.folds)
}

# now plot training and testing errors for all the degrees
matplot(x=1:dmax, y=cbind(rss.train.deg, rss.test.deg),
        type="b", pch=19, lty=1, cex=0.75, col=c("red", "blue"),
        main="RSS for Different Degrees", xlab="Degree", ylab="RSS")
legend("topright", legend=c("Training RSS", "Testing RSS"),
       lty=1, col=c("red", "blue"), pch=19, bty="n")

# results:
# choose degree 3; has lowest test RSS
which.min(rss.test.deg) 

# 4. use bs() to fit spline with df=4

bs.fit <- lm(nox ~ bs(dis, df=4))

x <- seq(min(dis), max(dis), length.out=100)
y <- predict(bs.fit, newdata=list(dis=x))

plot(dis, nox, cex=0.5)
lines(x, y, col="magenta", lwd=1.5)

# 5A. use bs() to fit splines using different dfs (and hence different ks)

df_min <- 3 # lowest df allowed by bs()
df_max <- 15 # try up to df=15

# to store rss for each df
rss.df <- rep(0, df_max) # locations 1 and 2 won't be used

for (i in df_min:df_max) {
  
  m <- lm(nox ~ bs(dis, df=i))
  rss.df[i] <- sum(m$residuals**2)
}

plot(x=df_min:df_max, y=rss.df[df_min:df_max],
     type="b", col="magenta", pch=16, lwd=1.5,
     main="RSS for Different DFs", xlab="df", ylab="RSS")

# 5B. use bs() with k-fold cross validation to choose a df

folds <- 5
n <- nrow(Boston)

# first assign each case to a fold
# choose from 1 to folds, n times
# this method isn't the best because it can result in some folds being
# bigger than others
set.seed(1)
folds.assigned <- sample(x=1:folds, size=n, replace=TRUE)

df_min <- 3 # lowest df allowed by bs()
df_max <- 15 # try up to df=15

# to store rss for each df
# locations 1 and 2 won't be used
rss.train.df <- rep(0, df_max)
rss.test.df <- rep(0, df_max)

for (df in df_min:df_max) {
  
  # to store error for each fold
  rss.train.folds <- vector(length=folds)
  rss.test.folds <- vector(length=folds)
  
  for (k in 1:folds) {
    
    # create train and test sets
    # folds.assigned != k returns a vector of TRUE/FALSE; use as a mask
    train.set <- Boston[folds.assigned != k, ] 
    test.set <- Boston[folds.assigned == k, ]
    
    # fit using train set
    fit <- lm(nox ~ bs(dis, df=df), data=train.set)
    
    # training error
    yhat.train <- predict(fit, newdata=train.set)
    rss.train.folds[k] <- sum((train.set$nox - yhat.train)^2)
    
    # testing error
    yhat.test <- predict(fit, newdata=test.set)
    rss.test.folds[k] <- sum((test.set$nox - yhat.test)^2)
  }
  
  # once the folds are done, compute overall error for the current df
  
  rss.train.df[df] <- mean(rss.train.folds)
  rss.test.df[df] <- mean(rss.test.folds)
}

# now plot training and testing errors for all the dfs

par(mfrow = c(1,2))

plot(x=df_min:df_max, y=rss.train.df[df_min:df_max],
     type="b", pch=19, lty=1, cex=0.75, col=c("red"),
     main="Training RSS for Different DFs\nWith 5-Fold CV",
     xlab="Degree", ylab="Training RSS")

plot(x=df_min:df_max, y=rss.test.df[df_min:df_max],
     type="b", pch=19, lty=1, cex=0.75, col=c("blue"),
     main="Testing RSS for Different DFs\nWith 5-Fold CV",
     xlab="Degree", ylab="Testing RSS")

# results:
# lowest test RSS is at index 4, which corresponds to df=6 (3 4 5 6)
# therefore choose df = 6
which.min(rss.test.df[df_min:df_max])




