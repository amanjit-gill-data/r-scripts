# 4.2 DEMO - M-FOLD CROSS VALIDATION
# do cross validation process 10 times per model i.e. for each model, do a 
# 5-fold cross-validation, 10 times
# This shows how much the MSE varies for each model due to the 
# train/test split.

library(faraway)
data("cheddar")

N <- nrow(cheddar)

# 5 folds
m <- 5

# do the 5-fold cross validation 10 times, to reduce sensitivity to which cases
# are put into the training/validation sets
nrep <- 10

# set up empty matrices for MSEs for all models
# 5 cols, one for each fold
# 10 rows, one for each repetition
# so 1 row represents 1 cross validation process
MSE.ace <- MSE.h2s <- MSE.lac <- MSE.all <- matrix(nrow=nrep, ncol=m)
MSE.ace.h2s <- MSE.ace.lac <- MSE.h2s.lac <- matrix(nrow=nrep, ncol=m)

# outer loop: run cross validation process 10 times
# inner loop: compute MSE when each fold is the validation set i.e. 5 times
for (i in 1:nrep) {

  # shuffle the N cases (rows) in the sample
  set.seed(122+i)
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
    MSE.ace[i,j] <- mean((val.set[,1] - pred.ace)^2)
    MSE.h2s[i,j] <- mean((val.set[,1] - pred.h2s)^2)
    MSE.lac[i,j] <- mean((val.set[,1] - pred.lac)^2)
    MSE.ace.h2s[i,j] <- mean((val.set[,1] - pred.ace.h2s)^2)
    MSE.ace.lac[i,j] <- mean((val.set[,1] - pred.ace.lac)^2)
    MSE.h2s.lac[i,j] <- mean((val.set[,1] - pred.h2s.lac)^2)
    MSE.all[i,j] <- mean((val.set[,1] - pred.all)^2)
  }
}

# we now have a 10x5 matrix for each model
# plot MSE for every model and join the points
# do this 10 times, once per repetition 

# first create the plot using the first repetition...
plot(
  c(
    mean(MSE.ace[1, ]),
    mean(MSE.h2s[1, ]),
    mean(MSE.lac[1, ]),
    mean(MSE.ace.h2s[1, ]),
    mean(MSE.ace.lac[1, ]),
    mean(MSE.h2s.lac[1, ]),
    mean(MSE.all[1, ])
  ),
  col=1, type="b", pch=16, xaxt="n", xlab="Model", ylab="MSE"
)

# ...then add the points for the remaining repetitions
for (i in 2:nrep) {
  points(
    c(
      mean(MSE.ace[i, ]),
      mean(MSE.h2s[i, ]),
      mean(MSE.lac[i, ]),
      mean(MSE.ace.h2s[i, ]),
      mean(MSE.ace.lac[i, ]),
      mean(MSE.h2s.lac[i, ]),
      mean(MSE.all[i, ])
    ),
    col=i, type="b", pch=16
  )
}

# add in custom x-axis labels
axis(
  side=1,
  at=1:7,
  labels=c("Acetic", "H2S", "Lactic", "Ace+H2S", "Ace+Lac", "H2S+Lac", "All")
)





