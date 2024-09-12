# WEEK 5 - CHECK YOUR UNDERSTANDING

# QUESTION (b)

# define function that works out discriminant for a given 
# observation and population 
disc <- function(mu, S, x, p) {
    t(mu) %*% solve(S) %*% x - 1/2 * t(mu) %*% solve(S) %*% mu + log(p)
}

# population means
mu1 <- rbind(1, 1)
mu2 <- rbind(1, 0)
mu3 <- rbind(0, 1)

# equal covariances
S <- rbind(c(1, 1/2), c(1/2, 1))

# equal priors
p <- 1/3

# observations
x1 <- rbind(1/5, 3/5)
x2 <- rbind(2, 4/5)
x3 <- rbind(3/4, 1)

# classify observation 1; result = class 3
disc(mu1, S, x1, p)
disc(mu2, S, x1, p)
disc(mu3, S, x1, p)

# classify observation 2; result = class 2
disc(mu1, S, x2, p)
disc(mu2, S, x2, p)
disc(mu3, S, x2, p)

# classify observation 3; result = class 1
disc(mu1, S, x3, p)
disc(mu2, S, x3, p)
disc(mu3, S, x3, p)

# QUESTION (c)

# plot lines separating the 3 regions
# see hand calcs for derivation

calc_a <- function(S, mu.i, mu.j) {
    t(mu.i) %*% solve(S) - t(mu.j) %*% solve(S)
}

calc_c <- function(S, mu.i, mu.j, p.i, p.j) {
    0.5 * t(mu.i) %*% solve(S) %*% mu.i 
        - 0.5 * t(mu.j) %*% solve(S) %*% mu.j 
        + log(p.j) - log(p.i)
}

x2 <- function(a1, a2, c, x1) {
    (c - a1 * x1)/a2
}
        

# plot the line separating regions 1 and 2

a <- calc_a(S, mu1, mu2)
a1 <- a[1]
a2 <- a[2]

c <- calc_c(S, mu1, mu2, 1/3, 1/3)

curve(c/a2 - a1*x1/a2,
      xname='x1', ylab='x2',
      xlim=c(-1,2), ylim=c(-1,2),
      cex.lab=0.8, cex.axis=0.8
      )


# plot the line separating regions 2 and 3

a <- calc_a(S, mu2, mu3)
a1 <- a[1]
a2 <- a[2]

c <- calc_c(S, mu2, mu3, 1/3, 1/3)

curve(c/a2 - a1*x1/a2, xname='x1', add=TRUE)

# plot the line separating regions 1 and 3

a <- calc_a(S, mu1, mu3)
a1 <- a[1]
a2 <- a[2]

c <- calc_c(S, mu1, mu3, 1/3, 1/3)

curve(c/a2 - a1*x1/a2, xname='x1', add=TRUE)



