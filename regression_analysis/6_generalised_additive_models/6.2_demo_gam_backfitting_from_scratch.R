# 6.2 GAM - BACKFITTING FROM SCRATCH
# demo using generated data

# SET UP

m <- 3 # no. variables; have 1 smooth term per variable, so there are m terms

set.seed(0)
dfs <- sample(4:6, size=m, replace=TRUE) # random df for each spline

n <- 10 # no. cases

set.seed(1)
x <- array(sample(1:10, size=n*m, replace=TRUE), dim=c(n, m)) # random x data

set.seed(2)
y <- sample(1:10, size=n) # random y data

# INITIALISE ITERATION

f.hat <- x*0 # estimated response to each term, for each case; n rows, m cols 

alpha <- mean(y)

rss.curr <- 0
rss.prev <- 10 # arbitrary; to ensure the script enters the loop at iteration 1

# ITERATE UNTIL CONVERGENCE

while (abs(rss.curr - rss.prev) > (1e-6)*rss.curr) {
  
  for (j in 1:m) {
    e.p <- y - alpha - rowSums(f.hat[, -j])
    fit <- smooth.spline(x[,j], e.p, df = dfs[j])
    f.hat[,j] <- predict(fit, x[,j])$y
  }
  
  rss.prev <- rss.curr
  rss.curr <- sum((y - rowSums(f.hat))^2)
}
f.hat

# REPEAT USING SOLUTION CODE TO CONFIRM CORRECTNESS

f<-x*0
alpha<-mean(y)
rss0 <- 0
ok <- TRUE
while (ok) { # backfitting loop
  for (i in 1 :m) { # loop through the smooth terms
    ep <- y - rowSums(f[,-i]) - alpha
    b <- smooth.spline(x[,i],ep,df=dfs[i])
    f[,i] <- predict(b,x[,i])$y
  }
  rss <- sum((y-rowSums(f))^2 )
  if (abs(rss-rss0)<1e-6*rss)
    ok <- FALSE
  rss0 <- rss
}


