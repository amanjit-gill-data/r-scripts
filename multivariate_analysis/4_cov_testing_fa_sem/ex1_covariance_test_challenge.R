# WEEK 4 DEMO - COVARIANCE TEST

library(dplyr)
library(readr)
library(purrr)
library(heplots)

pizza <- read.csv("datasets/pizza.csv")

mlm <- lm(cbind(prot, fat, ash, sodium, carb, cal) ~ brand, data=pizza)
summary(mlm)

boxM(mlm)

# RESULT: differences between all the covariances is highly significant.

