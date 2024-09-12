# WEEK 1: MULTIVARIATE NORMAL DISTRIBUTION - DEMO

library(mvtnorm)
library(GGally)
library(MVN)
library(dplyr)
library(readr)

# GENERATE AND VIEW MVN DATA

(mu <- c(2,3,4))

(sigma <- diag(1:3) + 1)

x <- rmvnorm(1000, mu, sigma)
dim(x)

ggpairs(as.data.frame(x))

# ASSESS MARGINAL NORMALITY (i.e. INDIVIDUAL NORMALITY)
# AND JOINT NORMALITY

# generate data for two dists, normal and binomial,
# and see if they're individually and jointly normal.
y <- rnorm(1000)
w <- rbinom(1000, 1, 1/2)
z <- cbind((2*w - 1)*y, y)

# this pairwise plot will show that marginally, both 
# dists are normal; but jointly, they are not.
ggpairs(as.data.frame(z))

# just to be sure, use diagnostics
# without, then with, the Mardia methods we learned.
# this shows that individually, the dists pass Shapiro-Wilk.
# but jointly, they fail Mardia's kurtosis.
mvn(z)
mvn(z, mvnTest = "mardia", univariateTest = "SW")

# can also view projections. for joint normality, 
# projections are normal.
zp1 <- z %*% c(1, -1)
plot(density(zp1))

xp1 <- x %*% c(1, -1, 0) # compare with this true mvn
plot(density(xp1))

xp2 <- x %*% rnorm(3) # and compare with a random projection
plot(density(xp2))

# TRY ALL THIS WITH REAL DATA
# 1. load data
# 2. pairwise plots
# 3. normality tests (default)
# 4. normality tests (mardia and sw)
# 5. try transformation to make it normal
# 6. redo steps 2-4 to see if transformation worked

ovens <- read_csv("Examples and Exercises/week1/provided files/ovens.csv")

ggpairs(ovens)

mvn(ovens)

mvn(ovens, mvnTest = "mardia", univariateTest = "SW")

ovens4 <- ovens^(1/4)
ggpairs(ovens4)

mvn(ovens4)

mvn(ovens4, mvnTest = "mardia", univariateTest = "SW")
