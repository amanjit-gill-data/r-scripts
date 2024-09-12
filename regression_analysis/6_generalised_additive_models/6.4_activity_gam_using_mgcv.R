# 6.4 GAM FITTING WITH MGCV()
# case study using Chicago data

# TASK NOTES:
# predict 'death', the daily death rate, based on pollution
# response follows poisson
# don't use pm25 predictor

library(gamair) # for Chicago data
library(mgcv) # for gam() and basis terms

data("chicago")

# PART 1. cubic regression spline for 'time', but linear terms for others

m1 <- gam(death ~ s(time, bs="cr",k=200)
          + pm10median
          + so2median
          + o3median
          + tmpd,
          data=chicago, family=poisson
)

par(mfrow=c(2,2)) 
gam.check(m1) # see pdf plot1a

par(mfrow=c(2,1))
plot(m1) # see pdf plot1b
plot(m1, residuals = TRUE) # see pdf plot1b

# outliers all occur around the same time
# so what is going on at that time?

which.max(chicago$tmpd) # max temp is at day 3116
which.max(chicago$pm10median) # max pm10 is at day 494
which.max(chicago$o3median) # max ozone is at day 3096
which.max(chicago$so2median) # max SO2 is at day 2577

plot(chicago$tmpd)
plot(chicago$o3median)
plot(chicago$death)

# observations:
# deaths occur around the time when there are high temps and high ozone

# PART 2. cubic regression splines for all included predictors

m2 <- gam(death ~ s(time,bs="cr",k=200)
          + s(pm10median, bs="cr")
          + s(so2median,bs="cr")
          + s(o3median,bs="cr")
          + s(tmpd,bs="cr"),
          data=chicago, family=poisson
)

par(mfrow=c(2,2)) 
gam.check(m2) # see pdf plot2

# observations:
# similar to m1

# PART 3. create lagged variables 

lagged.sums <- function(vec, sum.size) {
  
  n <- length(vec)
  
  lagged <- vector(length = n-sum.size)
  
  for (i in (sum.size+1):n) {
    lagged[i-sum.size] <- sum(vec[(i-sum.size):i])
  }
  
  return(lagged)
}

# replace all variables with lagged sums, except for death and time
pm10 <- lagged.sums(log(chicago$pm10median+40), 3) # log-transformed
o3 <- lagged.sums(chicago$o3median, 3)
so2 <- lagged.sums(log(chicago$so2median+10), 3) # log-transformed
tmp <- lagged.sums(chicago$tmpd, 3)

# remove first 3 values from death and time, to keep them lined up with
# the lagged variables

death <- chicago$death[4:length(chicago$death)]
time <- chicago$time[4:length(chicago$time)]

# PART 4. cubic regression spline for 'time', tensor product smooths for 
# both ozone & temp, and pm10 & temp
# so2 isn't used

m3 <- gam(death ~ s(time, bs="cr", k=200)
          + te(o3, tmp, k=8)
          + te(pm10, tmp, k=6), family=poisson
)

par(mfrow=c(2,2)) 
gam.check(m3) # see pdf plot3

# observations:
# better than before
# no more residuals sticking up on one side of the 0 line

# PART 5. cubic regression splines for "time" and pm10,
# tensor product smooth for o3 & temp
# so2 isn't used

m4 <- gam(death ~ s(time, bs="cr", k=200)
          + te(o3,tmp,k=8)
          + s(pm10,bs="cr", k=6),
          family=poisson
)

par(mfrow=c(2,2)) 
gam.check(m4) # see pdf plot4a

plot(m4) # see pdf plot4b

# observations:
# visually very similar to m3

# PART 6. choose best model

m1$gcv.ubre
m2$gcv.ubre
m3$gcv.ubre
m4$gcv.ubre

# observations:
# already ruled out m1 and m2 earlier
# m3 and m4 are very similar
# m3 has a slightly lower UBRE
# so for the lowest error, choose m3
# but for parsimony, choose the simpler model m4

