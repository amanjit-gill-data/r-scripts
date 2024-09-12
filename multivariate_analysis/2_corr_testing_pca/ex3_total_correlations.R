# WEEK 3 DEMO - TOTAL CORRELATIONS
# aim: to show equivalence between total correlation and regression

library(GGally)

x1 <- rnorm(100)
x2 <- rnorm(100) + x1

y <- x1*2 + x2 + rnorm(100)

ggpairs(data.frame(y, x1, x2))

# METHOD1 to compute R^2
# use summary from linear model object
# result: R^2 is 0.9379
summary(lmfit <- lm(y ~ x1 + x2))

# METHOD2 to compute R^2
# use linear model to predict y, then compute correlation between
# predicted y values and the y values that were used to build the model
# result: R^2 is 0.9379432
cor(predict(lmfit), y)^2

# METHOD3 to compute R^2
# use correlation matrix for the fitted data
# result: R^2 is 0.9379432
1 - 1/solve(cor(cbind(y, x1, x2)))[1,1]


