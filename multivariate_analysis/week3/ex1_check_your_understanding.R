# WEEK 3 CHECK YOUR UNDERSTANDING
# CANONICAL CORRELATIONS Q1-4

# ---------------------------------------------------------------------------- #
# Q1 - find canonical variables and correlation between first two and
# last three variables

S <- matrix(
    c(
        1, 0.4248, 0.0420, 0.0215, 0.0573,
        0.4248, 1, 0.1487, 0.2489, 0.2843,
        0.0420, 0.1487, 1, 0.6693, 0.4662,
        0.0215, 0.2489, 0.6693, 1, 0.6915,
        0.0573, 0.2843, 0.4662, 0.6915, 1
    ),
    5,5, byrow = TRUE
)

(S11 <- S[1:2, 1:2])
(S12 <- S[1:2, 3:5])
(S21 <- S[3:5, 1:2])
(S22 <- S[3:5, 3:5])

# USING CHOLESKY DECOMPOSITION FOR S22_neg_sqr

(S22_neg_sqr <- chol(solve(S22)))

(prod <- S22_neg_sqr %*% S21 %*% solve(S11) %*% S12 %*% t(S22_neg_sqr))

(eig <- eigen(prod))

# canonical correlation
(canon_corr <- sqrt(eig$values[1]))

# coefficients of canonical variables
(b <- t(S22_neg_sqr) %*% eig$vectors[,1])
(a<- canon_corr^-1 * solve(S11) %*% S12 %*% b)

# USING SPECTRAL DECOMPOSITION FOR S22_neg_sqr

(eig.S22 <- eigen(S22))

(V <- eig.S22$vectors)
(D <- matrix(
    c(
        eig.S22$values[1], 0, 0,
        0, eig.S22$values[2], 0,
        0, 0, eig.S22$values[3]
    ),
    3,3, byrow = TRUE
))

(S22_sqrt <- V %*% sqrt(D) %*% solve(V))
(S22_neg_sqrt <- solve(S22_sqrt))

(prod <- S22_neg_sqrt %*% S21 %*% solve(S11) %*% S12 %*% S22_neg_sqrt)

(eig <- eigen(prod))

# canonical correlation
(canon_corr <- sqrt(eig$values[1]))

# coefficients of canonical variables
(b <- t(S22_neg_sqrt) %*% eig$vectors[,1])
(a<- canon_corr^-1 * solve(S11) %*% S12 %*% b)

# ---------------------------------------------------------------------------- #
# Q2 - find canonical variables and correlation between first two and
# last three variables

S <- cbind(
    c(302.3,0,0,0,0),
    c(125.8,170.9,0,0,0),
    c(100.4,84.2,111.6,0,0),
    c(105.1,93.6,110.8,217.9,0),
    c(116.1,97.9,120.5,153.8,294.4))

# S is upper triangular; turn into full symmetric
(S<- S + t(S) - diag(diag(S)))

# partition S
(S11 <- S[1:2, 1:2])
(S12 <- S[1:2, 3:5])
(S21 <- S[3:5, 1:2])
(S22 <- S[3:5, 3:5])

# use Cholesky decomp to get inverse-sqrt of S22
(U <- chol(solve(S22)))

# find matrix product from which eigenvalues/vectors will be taken
(A <- U %*% S21 %*% solve(S11) %*% S12 %*% t(U))
(eig <- eigen(A))

# canonical correlation
(canon_corr <- sqrt(eig$values[1]))

# coeffs of canonical variables 
(b <- t(U) %*% eig$vectors[,1])
(a <- canon_corr^-1 * solve(S11) %*% S12 %*% b)

# ---------------------------------------------------------------------------- #
# Q4 - Verify that the first pair of canonical variables are just X2 
# and X3. Verify that the canonical correlation is 0.95.

(S <- matrix(
    c(
        100,0,0,0,
        0,1,.95,0,
        0,.95,1,0,
        0,0,0,100
    ), 4,4
))

S11 <- S[1:2, 1:2]
S12 <- S[1:2, 3:4]
S21 <- S[3:4, 1:2]
S22 <- S[3:4, 3:4]

(eig.S22 <- eigen(S22))

(V <- eig.S22$vectors)
(D <- matrix(
    c(
        eig.S22$values[1], 0,
        0, eig.S22$values[2]
    ),
    2,2, byrow = TRUE
))

(S22_sqrt <- V %*% sqrt(D) %*% solve(V))
(S22_neg_sqrt <- solve(S22_sqrt))

(prod <- S22_neg_sqrt %*% S21 %*% solve(S11) %*% S12 %*% S22_neg_sqrt)

(eig <- eigen(prod))

# canonical correlation
(canon_corr <- sqrt(eig$values[1]))

# coefficients of canonical variables
(b <- t(S22_neg_sqrt) %*% eig$vectors[,1])
(a<- canon_corr^-1 * solve(S11) %*% S12 %*% b)

# result: I got slightly different to expected:
# a: 0 -1
# b: -1 0

