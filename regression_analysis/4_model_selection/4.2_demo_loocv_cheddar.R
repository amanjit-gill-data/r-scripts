# 4.2 DEMO - LEAVE-ONE-OUT CROSS-VALIDATION

library(faraway)
data("cheddar")

# response: taste
# potential predictors: Acetic, H2S, Lactic

N <- nrow(cheddar)

# set up empty vectors for MSEs for all models
MSE.ace <- MSE.h2s <- MSE.lac <- MSE.all <- vector(length = N)
MSE.ace.h2s <- MSE.ace.lac <- MSE.h2s.lac <- vector(length = N)

for (i in 1:N) {
  train.set <- cheddar[-i,] # training set is all rows except current one
  val.set <- cheddar[i,] # validation set is just the current row

  # fit all models using the N-1 training cases
  
  lm.ace <- lm(taste ~ Acetic, data = train.set)
  lm.h2s <- lm(taste ~ H2S, data = train.set)
  lm.lac <- lm(taste ~ Lactic, data = train.set)
  lm.ace.h2s <- lm(taste ~ Acetic + H2S, data = train.set)
  lm.ace.lac <- lm(taste ~ Acetic + Lactic, data = train.set)
  lm.h2s.lac <- lm(taste ~ H2S + Lactic, data = train.set)
  lm.all <- lm(taste ~ ., data = train.set)
  
  # predict response for the 1 test case, for all models
  
  pred.ace <- predict(lm.ace, val.set)
  pred.h2s <- predict(lm.h2s, val.set)
  pred.lac <- predict(lm.lac, val.set)
  pred.ace.h2s <- predict(lm.ace.h2s, val.set)
  pred.ace.lac <- predict(lm.ace.lac, val.set)
  pred.h2s.lac <- predict(lm.h2s.lac, val.set)
  pred.all <- predict(lm.all, val.set)
  
  # compute MSE for the 1 test case, for all models
  
  MSE.ace[i] <- (val.set[,1] - pred.ace)^2
  MSE.h2s[i] <- (val.set[,1] - pred.h2s)^2
  MSE.lac[i] <- (val.set[,1] - pred.lac)^2
  MSE.ace.h2s[i] <- (val.set[,1] - pred.ace.h2s)^2
  MSE.ace.lac[i] <- (val.set[,1] - pred.ace.lac)^2
  MSE.h2s.lac[i] <- (val.set[,1] - pred.h2s.lac)^2
  MSE.all[i] <- (val.set[,1] - pred.all)^2
}

# plot MSE for every model
plot(c(
  mean(MSE.ace),
  mean(MSE.h2s),
  mean(MSE.lac),
  mean(MSE.ace.h2s),
  mean(MSE.ace.lac),
  mean(MSE.h2s.lac),
  mean(MSE.all)),
  xaxt="n", xlab="Models", ylab="MSE", col="red", type="b", pch=16
)
axis(
  side=1,
  at=1:7,
  labels=c("Acetic", "H2S", "Lactic", "Ace+H2S", "Ace+Lac", "H2S+Lac", "All")
)



