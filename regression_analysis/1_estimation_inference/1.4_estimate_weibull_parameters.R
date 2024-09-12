# 1.4 Estimation of Weibull Parameters

### DATA ###

p.vessel.lifetimes <- c(1051, 1337, 1389, 1921, 1942, 2322, 3629, 4006, 4012, 
              4063, 4921, 5445, 5620, 5817, 5905, 5956, 6068, 6121, 
              6473, 7501, 7886, 8108, 8546, 8666, 8831, 9106, 9711, 
              9806, 10205, 10396, 10861, 11026, 11214, 11362, 11604,
              11608, 11745, 11762, 11895, 12044, 13520, 13670, 14110, 
              14496, 15395, 16179, 17092, 17568, 17568)

### EXERCISES ###

# plot histogram of pressure vessel lifetimes

hist(p.vessel.lifetimes, nclass=17, xlim=c(0, 20000), 
     xlab="time to failure (hours)", main="Pressure Vessel Lifetimes")

# estimate parameter theta, when parameter lambda is given

lambda <- 2

theta_hat <- function(Y_vect, lambda) {
  (sum(Y_vect ** lambda) / length(Y_vect)) ** (1/lambda)
}

theta_hat(p.vessel.lifetimes, lambda)


