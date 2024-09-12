# WEEK 5 CHALLENGE - DISCRIMINANT ANALYSIS

library(MASS)
library(dplyr)
library(heplots)

pizza <- read.csv("Examples and Exercises/datasets/pizza.csv")

# TASK 1: COMPARE LDA AND QDA FOR PREDICTING BRAND

# model
# predict
# confusion matrix
l.classifier <- lda(brand ~ ., data=pizza)
l.pred <- predict(l.classifier)$class
(l.cm <- table(truth=pizza$brand, prediction=l.pred))

# model
# predict
# confusion matrix
q.classifier <- qda(brand ~ ., data=pizza)
q.pred <- predict(q.classifier)$class
(q.cm <- table(truth=pizza$brand, prediction=q.pred))

# error rates
# result: QDA error rate is lower. QDA is better.
(l.error <- 1 - sum(diag(l.cm))/sum(l.cm))
(q.error <- 1 - sum(diag(q.cm))/sum(q.cm))

# TASK 2 - COMPARE USING CROSS-VALIDATION

# model
# confusion matrix
l.classifier <- lda(brand ~ ., data=pizza, CV=TRUE)
(l.cm <- table(truth=pizza$brand, prediction=l.classifier$class))

# model
# confusion matrix
q.classifier <- qda(brand ~ ., data=pizza, CV=TRUE)
(q.cm <- table(truth=pizza$brand, prediction=q.classifier$class))

# error rates
# result: LDA error rate is lower. 
(l.error <- 1 - sum(diag(l.cm))/sum(l.cm))
(q.error <- 1 - sum(diag(q.cm))/sum(q.cm))

# TASK 3 - MAKE PREDICTION FOR ONE NEW OBSERVATION
# result: new obs belongs to brand J

(x.new <- data.frame(prot=12, fat=15, ash=1.5, sodium=0.6, carb=25, cal=3))

l.classifier <- lda(brand ~ ., data=pizza)
q.classifier <- qda(brand ~ ., data=pizza)

predict(l.classifier, newdata=x.new)
predict(q.classifier, newdata=x.new)


