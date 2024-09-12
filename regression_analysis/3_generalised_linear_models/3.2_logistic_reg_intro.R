# 3.2 Logistic Regression - Introductory Plots

library(ISLR)
data("Default")
attach(Default)

# turn Yes/No into 1/0
default.bin <- rep(0, length(default))
default.bin[default == "Yes"] <- 1

# linear and logistic models
lm.fit <- lm(default.bin ~ balance)
log.fit <- glm(default.bin ~ balance, family = "binomial")

# plot on same axes
plot(balance, default.bin, pch = "|", col="orange", ylab = "P(default)")
abline(h = c(0, 1), lty="dashed")
o <- order(balance) # to make the lines join the points together nicely
lines(balance[o], lm.fit$fitted.values[o], pch=".", lwd=2, col="red")
lines(balance[o], log.fit$fitted.values[o], pch=".", lwd=2, col="blue")

