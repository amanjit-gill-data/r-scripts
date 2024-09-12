# 1.4 Likelihood Maximisation

### DATA ### 
p.vessel.lifetimes <- c(1051, 1337, 1389, 1921, 1942, 2322, 3629, 4006, 4012, 
                        4063, 4921, 5445, 5620, 5817, 5905, 5956, 6068, 6121, 
                        6473, 7501, 7886, 8108, 8546, 8666, 8831, 9106, 9711, 
                        9806, 10205, 10396, 10861, 11026, 11214, 11362, 11604,
                        11608, 11745, 11762, 11895, 12044, 13520, 13670, 14110, 
                        14496, 15395, 16179, 17092, 17568, 17568)

### FUNCTION DEF ###

# log-likelihood for weibull dist, given lambda and theta
logl <- function(Y_vect, lambda, theta) {
  return(sum((lambda-1)*log(Y_vect) + log(lambda) - lambda*log(theta) - (Y_vect/theta)**lambda))
}

### EXERCISE ###

# vector of x-values - thetas
thetas <- seq(7000, 13000, 100)

# vector of y-values - log-likelihood for each theta, when lambda = 2
lls <- c()
for (theta in thetas) {
  lls <- c(lls, logl(p.vessel.lifetimes, 2, theta))
}

plot(x=thetas, y=lls, type="l")

# coordinates of max log-likelihood
y_max_ll <- max(lls)
x_max_ll <- thetas[which.max(lls)]

# add lines to mark maximum
abline(h = y_max_ll, lty="dashed", col="red")
abline(v = x_max_ll, lty="dashed", col="red")

