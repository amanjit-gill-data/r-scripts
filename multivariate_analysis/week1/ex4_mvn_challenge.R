# WEEK 1 - MULTIVARIATE NORMAL DISTRIBUTION - CHALLENGE

library(mvtnorm)
library(GGally)
library(MVN)
library(dplyr)
library(readr)

wine <- read_csv(
    "Examples and Exercises/week1/provided files/winequality-red.extract7.csv")

# TASK 1:
# pairwise plots
# assess normality visually
# hypothesis tests using mvn()

ggpairs(wine)
# density and pH look normal
# volatile acidity looks borderline
# all the others do not look normal

mvn(wine)
mvn(wine, mvnTest = "mardia", univariateTest = "SW")
# multivariate normality = no
# marginal normality = all no

# TASK 2:
# find a transformation to make the variables normal
# assess normality to make sure

wine_sqrt <- sqrt(wine)
ggpairs(wine_sqrt)
mvn(wine_sqrt, mvnTest = "mardia", univariateTest = "SW")
# square root transformation didn't help much
# mardia and SW still NO

wine_ln <- log(wine, exp(1))
ggpairs(wine_ln)
mvn(wine_ln, mvnTest = "mardia", univariateTest = "SW")
# loge transformation helped more, but not enough
# mardia and SW still NO

# Provided solution
# this shows how to transform the variables individually
# this is better than applying the same transformation to all
# but still fails mvn() tests
# still normal-ish enough to use in multivariate analysis 
wine.t <- wine %>% mutate(
    `log sulphates`=log(`sulphates`),
    `root4 free sulfur dioxide` = (`free sulfur dioxide`)^(1/4), 
    `root4 total sulfur dioxide` = (`total sulfur dioxide`)^(1/4), 
    `log fixed acidity`=log(`fixed acidity`), 
    `log volatile acidity`=log(`volatile acidity`), 
    .keep="unused")

ggpairs(wine.t)
mvn(wine.t, mvnTest = "mardia", univariateTest = "SW")

