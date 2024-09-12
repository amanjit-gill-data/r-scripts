# 1.4 Newton-Raphson Algorithm in 1D

# recursive function to estimate Poisson parameter theta
# parameter lambda is held constant
# computes U and U' on the random vector, using original/old theta
# then computes new theta
# if this is the last iteration, returns new theta
# otherwise function is run again, using new theta
estimate_theta <- function(Y_vect, theta, lambda, iters) {
  
  N <- length(Y_vect)
  U <- -lambda*N/theta + lambda*sum(Y_vect**lambda)/(theta**(lambda+1))
  U_prime <- lambda*N/(theta**2) - lambda*(lambda+1)*sum(Y_vect**lambda)/(theta**(lambda+2))
  new_theta <- theta - U/U_prime
  
  if (iters == 1) {
    return(new_theta)
  }
  else {
    estimate_theta(Y_vect, new_theta, lambda, iters-1)
  }
}

# test out the function on the pressure vessel data; 50 iterations
pressure <- c(1051, 1337, 1389, 1921, 1942, 2322, 3629, 4006, 4012, 
              4063, 4921, 5445, 5620, 5817, 5905, 5956, 6068, 6121, 
              6473, 7501, 7886, 8108, 8546, 8666, 8831, 9106, 9711, 
              9806, 10205, 10396, 10861, 11026, 11214, 11362, 11604,
              11608, 11745, 11762, 11895, 12044, 13520, 13670, 14110, 
              14496, 15395, 16179, 17092, 17568, 17568)

estimate_theta(pressure, theta=1, lambda=2, iters=50)


