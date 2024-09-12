# WEEK 3 DEMO - CANONICAL CORRELATONS

library(dplyr)
library(GGally)
library(CCA)
library(CCP)

'weight waist pulse chins situps jumps
   191  36  50   5  162   60
   189  37  52   2  110   60
   193  38  58  12  101  101
   162  35  62  12  105   37
   189  35  46  13  155   58
   182  36  56   4  101   42
   211  38  56   8  101   38
   167  34  60   6  125   40
   176  31  74  15  200   40
   154  33  56  17  251  250
   169  34  50  17  120   38
   166  33  52  13  210  115
   154  34  64  14  215  105
   247  46  50   1   50   50
   193  36  46   6   70   31
   202  37  62  12  210  120
   176  37  54   4   60   25
   157  32  52  11  230   80
   156  33  54  15  225   73
   138  33  68   2  110   43' %>%
    textConnection() %>% read.table(header=TRUE) -> fitness

summary(fitness)
ggpairs(fitness)

# CANONICAL CORRELATION ANALYSIS

X <- fitness[1:3]
Y <- fitness[4:6]
print(fitness.cc <- cc(X,Y))
plt.cc(fitness.cc, var.label = TRUE)

# COMPARE TO FORMULAS USING CHOLESKY DECOMP METHOD

S <- cov(fitness[,c(4,5,6,1,2,3)])
S11 <- S[1:3, 1:3]
S12 <- S[1:3, 4:6]
S21 <- S[4:6, 1:3]
S22 <- S[4:6, 4:6]

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

# NOW USE SPECTRAL DECOMP METHOD

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

# STANDARDISED CANONICAL CORRELATIONS
# useful for comparing correlations within the same group
# this actually computes the z-score (x- xbar)/s for each value

Xs <- scale(X)
Ys <- scale(Y)
print(fitness.ccs <- cc(Xs, Ys))

# HYPOTHESIS TESTING USING VARIOUS METHODS FROM TOPIC 3
# using CCP package

n <- nrow(fitness)
p <- ncol(X)
q <- ncol(Y)

p.asym(fitness.cc$cor, n, p, q, tstat="Wilks")
p.asym(fitness.cc$cor, n, p, q, tstat="Hotelling")
p.asym(fitness.cc$cor, n, p, q, tstat="Pillai")
p.asym(fitness.cc$cor, n, p, q, tstat="Roy")

# PERMUTATION TESTING
# THESE ARE TESTS THAT WORK EVEN IF DATA AREN'T NORMAL

p.perm(X, Y, type="Wilks")
p.perm(X, Y, type="Hotelling")
p.perm(X, Y, type="Pillai")
p.perm(X, Y, type="Roy")

# CCA WHEN ONLY CORRELATION MATRIX IS AVAILABLE

# set up correlation matrix

" 1.0      .505   .569   .602
   .505   1.0     .422   .467
   .569   .422   1.0     .926
   .602   .467   .926   1.0" %>% 
    textConnection() %>% scan() %>%
    matrix(4,4, byrow=TRUE) -> chicken

rownames(chicken) <- colnames(chicken) <- c("head1","head2","leg1","leg2")

# do CCA, using spectral decomp method for S22^-1/2

S11 <- chicken[1:2, 1:2]
S12 <- chicken[1:2, 3:4]
S21 <- chicken[3:4, 1:2]
S22 <- chicken[3:4, 3:4]

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







