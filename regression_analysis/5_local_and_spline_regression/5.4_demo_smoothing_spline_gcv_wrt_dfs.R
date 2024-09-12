# 5.4 SMOOTHING SPLINES - GCV - SELECT DF
# demo using fossil data

fossil <- read.table("fossil.txt", header=TRUE)
attach(fossil)

# METHOD 1: TRY LOTS OF DFS AND CHOOSE THE ONE WITH SMALLEST GCV ERROR

num.df <- 100 # no. effective dfs to try
min.df <- 8 # min and max effective dfs to try
max.df <- 20
dfs <- seq(min.df, max.df, length.out=num.df)

n <- nrow(fossil)

gcv.errors <- vector(length=num.df)

for (i in 1:num.df) {
  
  df <- dfs[i]
  fit <- smooth.spline(age, strontium.ratio, df=df)
  rss <- sum((strontium.ratio - predict(fit, age)$y)^2)
  gcv.errors[i] <- (1/n) * rss / ((1 - df/n)^2)
}

plot(dfs, gcv.errors, type="l", main="GCV Error Vs Effective DF")
min.gcv.index <- which.min(gcv.errors)
abline(v = dfs[min.gcv.index], lty=2)

# METHOD 2: LET SMOOTH.SPLINE(with 'cv' left as FALSE) CHOOSE BEST DF

fit <- smooth.spline(age, strontium.ratio) # cv is FALSE by default
fit$df # reveal the best df (that yields the best GCV error)

