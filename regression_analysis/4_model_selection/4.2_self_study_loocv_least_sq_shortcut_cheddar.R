# 4.2 SELF-STUDY - LEAVE-ONE-OUT CROSS VALIDATION
# repeat the model comparison, using the least-squares version

library(faraway)
data("cheddar")

N <- nrow(cheddar)

# fit models on all N cases
lm.ace <- lm(taste ~ Acetic, data = cheddar)
lm.h2s <- lm(taste ~ H2S, data = cheddar)
lm.lac <- lm(taste ~ Lactic, data = cheddar)
lm.ace.h2s <- lm(taste ~ Acetic + H2S, data = cheddar)
lm.ace.lac <- lm(taste ~ Acetic + Lactic, data = cheddar)
lm.h2s.lac <- lm(taste ~ H2S + Lactic, data = cheddar)
lm.all <- lm(taste ~ ., data = cheddar)

# get hat values i.e. leverages for all N cases
hat.ace <- hatvalues(lm.ace)
hat.h2s <- hatvalues(lm.h2s)
hat.lac <- hatvalues(lm.lac)
hat.ace.h2s <- hatvalues(lm.ace.h2s)
hat.ace.lac <- hatvalues(lm.ace.lac)
hat.h2s.lac <- hatvalues(lm.h2s.lac)
hat.all <- hatvalues(lm.all)

# get weighted MSE for each model
# this shortcut works for any linearly fitted model using lm()
# it just needs to be linear in the coefficients, not the variables?
mse.ace <- sum(((cheddar$taste - lm.ace$fitted.values) / (1 - hat.ace))^2)/N
mse.h2s <- sum(((cheddar$taste - lm.h2s$fitted.values) / (1 - hat.h2s))^2)/N
mse.lac <- sum(((cheddar$taste - lm.lac$fitted.values) / (1 - hat.lac))^2)/N
mse.ace.h2s <- sum(((cheddar$taste - lm.ace.h2s$fitted.values) / (1 - hat.ace.h2s))^2)/N
mse.ace.lac <- sum(((cheddar$taste - lm.ace.lac$fitted.values) / (1 - hat.ace.lac))^2)/N
mse.h2s.lac <- sum(((cheddar$taste - lm.h2s.lac$fitted.values) / (1 - hat.h2s.lac))^2)/N
mse.all <- sum(((cheddar$taste - lm.all$fitted.values) / (1 - hat.all))^2)/N

# plot mse vs model
plot(
  c(mse.ace, mse.h2s, mse.lac, mse.ace.h2s, mse.ace.lac, mse.h2s.lac, mse.all),
  xaxt="n", xlab="Models", ylab="MSE", col="red", type="b", pch=16
)
axis(
  side=1, at=1:7,
  labels=c("Acetic", "H2S", "Lactic", "Ace+H2S", "Ace+Lac", "H2S+Lac", "All")
)

