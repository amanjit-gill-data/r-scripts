# WEEK 5 - DEMO - SUPPORT VECTOR MACHINES

library(e1071)
library(pROC)
library(dplyr)

data(iris)

# TWO-CLASS SVM
# classes: virginica, versicolor
# variables: sepal length, sepal width

# first visualise the data
# (copied from demo to save time)

iris2 <- iris %>% filter(Species!="setosa") %>% select(Species,Sepal.Length,Sepal.Width) %>% mutate(Species=factor(Species))
plot(iris2[,2:3],col=as.numeric(iris$Species)+2)
legend("topleft", levels(iris$Species)[-1], fill=3:4, ncol=1)

# fit the SVM with linear kernel
svm.linear <- svm(Species ~ ., data=iris2, kernel="linear")
plot(svm.linear, data=iris2)

# fit the SVM with radial kernel, different gammas
svm.radial <- svm(Species~., data=iris2, kernel="radial")
plot(svm.radial, data=iris2)

svm.radial1 <- svm(Species~., data=iris2, kernel="radial", gamma=1)
plot(svm.radial1, data=iris2)

svm.radial10 <- svm(Species~., data=iris2, kernel="radial", gamma=10)
plot(svm.radial10, data=iris2)

svm.radial100 <- svm(Species~., data=iris2, kernel="radial", gamma=100)
plot(svm.radial100, data=iris2)

# fit the SVM with radial kernel, high cost
svm.radialC100 <- svm(Species~.,data=iris2,kernel="radial",cost=100)
plot(svm.radialC100, data=iris2)

# OBSERVATIONS

# lower gamma and low cost:
# more like linear
# fewer support vectors
# contiguous prediction regions
# high in-sample misclassification rate

# higher gamma and high cost:
# more complex prediction regions, possibly islands
# many support vectors
# possibly high out-of-sample misclassification rate due to overfitting

# COMPARE CONFUSION MATRICES

table(iris2$Species, fitted(svm.radial))

table(iris2$Species,fitted(svm.radial100))

# CROSS-VALIDATION
# to give an idea of what the out-of-sample error rate might be
# use 10-fold i.e. dataset is divided into 10 parts
# result: it's not better to set gamme very high

svm.radial <- svm(Species ~ ., data=iris2, kernel="radial", cross=10)
summary(svm.radial)

svm.radial100 <- svm(Species ~ ., data=iris2, kernel="radial", gamma=100, cross=10)
summary(svm.radial100)

# AUTOMATED TUNING
# $best.model is the model with the smallest error.
tuned.svm <- tune.svm(Species ~ ., data=iris2, kernel="radial", gamma=10^(-1:1), cost=10^(-1:1))
summary(tuned.svm)
tuned.svm$best.model

# MULTICLASS SVM (MORE THAN 2 POPULATIONS)
# use all the classes in iris, not just virginica and versicolor

svm3.radial <- svm(Species~Sepal.Length+Sepal.Width,data=iris,cross=10)
summary(svm3.radial)
plot(svm3.radial, data=iris, Sepal.Length ~ Sepal.Width)

# PREDICTION

x.new <- data.frame(Sepal.Width=3.4, Sepal.Length=6.0)

predict(svm3.radial, newdata=x.new, decision.values=TRUE)

# ROC CURVES TO SET THRESHOLDS

y.hat.radial <- predict(svm.radial, newdata=iris2, decision.values=TRUE)
y.hat.radial <- c(attr(y.hat.radial, "decision.values"))
hist(y.hat.radial)

roc.radial <- roc(iris2$Species=="versicolor", y.hat.radial)

y.hat.linear <- predict(svm.linear, newdata=iris2, decision.values=TRUE)
y.hat.linear <- c(attr(y.hat.linear, "decision.values"))

roc.linear <- roc(iris2$Species=="versicolor", y.hat.linear)

plot(roc.radial, col=2)
plot(roc.linear, col=3, add=TRUE)
legend("bottomright",c("RBF SVM", "Linear SVM"),lty=1,col=2:3)



