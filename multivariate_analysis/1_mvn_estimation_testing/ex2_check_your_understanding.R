# MULTIVARIATE DISTRIBUTION
# Check your understanding

# Q1

pchisq(2.41, 2)

# Q2 

A <- rbind(c(1,1,1), c(1,-2,1))
mu <- cbind(c(3, -1, 2))
sigma <- rbind(c(3,2,1), c(2,3,1), c(1,1,2))

A %*% mu

A %*% sigma %*% t(A)
