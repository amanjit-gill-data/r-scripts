# WEEK 5 CHALLENGE - SUPPORT VECTOR MACHINES

library(e1071)
library(pROC)
library(dplyr)

pizza <- read.csv("Examples and Exercises/datasets/pizza.csv") %>% mutate(brand=factor(brand))

# TASK 1: SELECT AND TUNE SVM TO CLASSIFY BRAND
# CALCULATE CROSS-VALIDATED ACCURACY

# try radial

svm.tuned.rad <- tune.svm(brand ~ ., data=pizza, kernel="radial", gamma=10^(-1:1), cost=10^(-1:1))
summary(svm.tuned.rad)

best.tuned.rad <- svm.tuned.rad$best.model
summary(best.tuned.rad)

# try linear

svm.tuned.lin <- tune.svm(brand ~ ., data=pizza, kernel="linear", cost=10^(-1:1))
summary(svm.tuned.lin)

best.tuned.lin <- svm.tuned.lin$best.model
summary(best.tuned.lin)

# TASK 2: USE SELECTED SVM TO CLASSIFY A NEW PIZZA

x.new <- data.frame(prot=12, fat=15, ash=1.5, sodium=0.6, carb=25, cal=3)

predict(best.tuned.rad, newdata=x.new, decision.values=TRUE)



