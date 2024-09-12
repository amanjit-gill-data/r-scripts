# 4.1 SELF-STUDY - MSE

library(faraway)
library(Metrics)
data("cheddar")

n <- nrow(cheddar)

# visually confirm linear relationship between RV and EVs
plot(cheddar)

m <- lm(taste ~ ., data=cheddar)
summary(m)

mse.manually <- sum((cheddar$taste - m$fitted.values)^2) / n

mse.manually.resids <- sum(m$residuals^2) / n

mse.func <- mse(cheddar$taste, m$fitted.values)

