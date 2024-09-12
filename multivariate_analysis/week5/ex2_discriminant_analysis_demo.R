# WEEK 5 DEMO - DISCRIMINANT ANALYSIS 

library(MASS)
library(dplyr) # load this after MASS to override MASS's select() function.
library(heplots)

data(iris)

# LINEAR DISCRIMINANT ANALYSIS
# equal covariances - can use lda()

# with all variables 
(iris.lda <- lda(Species ~ ., data=iris))

# with only two variables
(iris.lda2 <- lda(Species ~ Sepal.Length + Sepal.Width, data=iris))

# VISUALISATION
# (copied from demo)

# first create a grid of values covering the space
SLs <- seq(min(iris$Sepal.Length), max(iris$Sepal.Length), length.out=200)
SWs <- seq(min(iris$Sepal.Width), max(iris$Sepal.Width), length.out=200)
xy <- expand.grid(Sepal.Length=SLs, Sepal.Width=SWs)

# allocate new observations to classes
z <- factor(predict(iris.lda2, newdata=xy)$class) 

# plot class allocations onto grid, coloured by actual class
plot(xy[,1],xy[,2], col=z, xlab="Sepal Length",ylab="Sepal Width", pch=".")
points(iris$Sepal.Length,iris$Sepal.Width, col=factor(iris$Species), pch=15)

# TEST EQUALITY OF VARIANCES
# result: variances are NOT equal
# can still use LDA; might work anyway
select(iris, -Species) %>% boxM(iris$Species)

# QUADRATIC DISCRIMINANT ANALYSIS
# ONLY TWO PREDICTORS
# use qda()

(iris.qda2 <- qda(Species ~ Sepal.Length + Sepal.Width, data=iris))

# VISUALISATION

# retain grid from previous visualisation

# allocate new obs to classes
z <- factor(predict(iris.qda2, newdata=xy)$class)

# plot class allocations onto grid, coloured by actual class
plot(xy[,1],xy[,2], col=z, xlab="Sepal Length",ylab="Sepal Width", pch=".")
points(iris$Sepal.Length,iris$Sepal.Width, col=factor(iris$Species), pch=15) 

# looks similar to the LDA. zoom out...
# result: classes have strange shapes, especially outside the data range.

SLs <- seq(2, 10, length.out=200)
SWs <- seq(0, 6, length.out=200)
xy <- expand.grid(Sepal.Length=SLs, Sepal.Width=SWs)

# Make the predictions:
z <- factor() 

plot(xy[,1],xy[,2], col=z, xlab="Sepal Length",ylab="Sepal Width", pch=".")
points(iris$Sepal.Length,iris$Sepal.Width, col=factor(iris$Species), pch=15)

# QDA WITH ALL PREDICTORS
(iris.qda <- qda(Species ~ ., data=iris))

# COMPARE CLASSIFIERS WITH CONFUSION MATRIX
# result: lda and qda perform similarly for this problem

# lda (all predictors)
table(truth=iris$Species, prediction=predict(iris.lda)$class)

# qda (all predictors)
table(truth=iris$Species, prediction=predict(iris.qda)$class)

# lda (sepal only)
table(truth=iris$Species, prediction=predict(iris.lda2)$class)

# qda (sepal only)
table(truth=iris$Species, prediction=predict(iris.qda2)$class)

# CROSS-VALIDATION
# each observation gets a turn being predicted, based on the rest
# of the dataset.
# result: LDA is slightly better than QDA.

# lda (all predictors)
table(truth=iris$Species, prediction=lda(Species ~ ., data=iris, CV=TRUE)$class)

# qda (all predictors)
table(truth=iris$Species, prediction=qda(Species ~ ., data=iris, CV=TRUE)$class)

# lda (sepal only)
table(truth=iris$Species, prediction=lda(Species ~ Sepal.Length + Sepal.Width, data=iris, CV=TRUE)$class)

# qda (sepal only)
table(truth=iris$Species, prediction=qda(Species ~ Sepal.Length + Sepal.Width, data=iris, CV=TRUE)$class)


