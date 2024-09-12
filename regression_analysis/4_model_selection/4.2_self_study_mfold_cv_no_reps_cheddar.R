# 4.2 SELF-STUDY - M-FOLD CROSS VALIDATION
# do cross validation process only once per model i.e. for each model, do a 
# 5-fold cross-validation; do not repeat this
# then the MSE for each model is the mean MSE for the 5 folds

library(faraway)
data("cheddar")

N <- nrow(cheddar)

# 5 folds
m <- 5

# Ed demo does 5-fold cross validation 10 times for each model, 
# to get an average MSE that will hopefully be better than just doing the 
# 5-fold cross validation just once

# but i'll start with doing the 5-fold cross validation just once, to understand
# the process better

# set up empty vectors for MSEs for all models
MSE.ace <- MSE.h2s <- MSE.lac <- MSE.all <- vector(length = m)
MSE.ace.h2s <- MSE.ace.lac <- MSE.h2s.lac <- vector(length = m)

# shuffle the N cases (rows) in the sample
set.seed(123)
shuffle.ids <- sample(1:N, size=N, replace=FALSE) # create numbers 1-30, ordered randomly
cheddar.shuffled <- cheddar[shuffle.ids, ] # shuffle data, then the next val set is skimmed off the top

seq.id <- round(seq(0,N,by=N/m),0)

for (j in 1:m) {
  
  val.ids <- (seq.id[j]+1):seq.id[j+1] # vector indices of new val set
  val.set <- cheddar.shuffled[val.ids, ]
  train.set <- cheddar.shuffled[-val.ids, ]
  
  # fit all models using the training set
  
  lm.ace <- lm(taste ~ Acetic, data = train.set)
  lm.h2s <- lm(taste ~ H2S, data = train.set)
  lm.lac <- lm(taste ~ Lactic, data = train.set)
  lm.ace.h2s <- lm(taste ~ Acetic + H2S, data = train.set)
  lm.ace.lac <- lm(taste ~ Acetic + Lactic, data = train.set)
  lm.h2s.lac <- lm(taste ~ H2S + Lactic, data = train.set)
  lm.all <- lm(taste ~ ., data = train.set)
  
  # predict responses for the validation set, for all models
  
  pred.ace <- predict(lm.ace, val.set)
  pred.h2s <- predict(lm.h2s, val.set)
  pred.lac <- predict(lm.lac, val.set)
  pred.ace.h2s <- predict(lm.ace.h2s, val.set)
  pred.ace.lac <- predict(lm.ace.lac, val.set)
  pred.h2s.lac <- predict(lm.h2s.lac, val.set)
  pred.all <- predict(lm.all, val.set)
  
  # compute MSE for val set, for all models
  # each vector will contain 5 MSEs, one for each fold as the val set
  MSE.ace[j] <- mean((val.set[,1] - pred.ace)^2)
  MSE.h2s[j] <- mean((val.set[,1] - pred.h2s)^2)
  MSE.lac[j] <- mean((val.set[,1] - pred.lac)^2)
  MSE.ace.h2s[j] <- mean((val.set[,1] - pred.ace.h2s)^2)
  MSE.ace.lac[j] <- mean((val.set[,1] - pred.ace.lac)^2)
  MSE.h2s.lac[j] <- mean((val.set[,1] - pred.h2s.lac)^2)
  MSE.all[j] <- mean((val.set[,1] - pred.all)^2)
  
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
  xaxt="n", xlab="Models", ylab="MSE", col="red", type="b", pch=16,
  main="5-fold cross validation\neach point is the mean MSE for 5 folds"
)
axis(
  side=1,
  at=1:7,
  labels=c("Acetic", "H2S", "Lactic", "Ace+H2S", "Ace+Lac", "H2S+Lac", "All")
)

