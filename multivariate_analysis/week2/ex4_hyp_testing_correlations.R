# WEEK 2 DEMO - HYPOTHESIS TESTING CORRELATIONS

library(ggm)

# X1 = intelligence
# X2 = weight
# X3 = age

# ORDINARY CORRELATION
# between weight and intelligence
# h0: rho = 0 i.e. no correlation between them
# result: t_test > t_crit, therefore reject h0
# weight and intelligence are correlated

n <- 20
alpha <- 0.05

R <- matrix(
    c(
        1, .6162, .8267,
        .6162, 1, .7321,
        .8267, .7321, 1
    ),
    3,3
)

r12 <- R[1,2]

(t_test <- r12 * sqrt((n-2)/(1 - r12**2)))
(t_crit <- qt(1-alpha, n-2))

# ORDINARY CORRELATION
# ALTERNATE METHOD USING 2-SIDED P-VALUE
# result: p-value < 0.05, therefore reject h0

(p_value <- 2 * pt(r12 * sqrt((n-2)/(1 - r12**2)), n-2, lower.tail = FALSE))

# ORDINARY CORRELATION
# ALTERNATE METHOD USING FISHER'S Z-APPROXIMATION
# result: Z_approx > norm_crit, so reject h0

(Z_approx <- 0.5 * log((1+r12)/(1-r12), exp(1)))
(norm_crit <- qnorm(1-alpha, 0, 1/sqrt(n-3)))

# ORDINARY CORRELATION
# ALTERNATE METHOD USING T-TEST FUNCTION

pcor.test(r12, 0, 20)

# PARTIAL CORRELATION
# between weight and intelligence, given age
# this demo uses p-value
# result: p_value > 0.05, so do NOT reject h0

(r12.3 <- pcor(c(1,2,3), R))

(Z_approx <- 0.5 * log((1+r12.3)/(1-r12.3), exp(1)))
(p_value <- 2 * pnorm(Z_approx, 0, sqrt(1/(n-4)), lower.tail = FALSE))

# PARTIAL CORRELATION
# ALTERNATE METHOD USING T-TEST FUNCTION
# result: p_value > 0.05, so do NOT reject h0

pcor.test(r12, 1, 20)

