# 3.2 Hosmer-Lemeshow Statistic - Trade Union Example
# calculate statistic (both maths and glm) and do hyp test

library(SemiPar)
library(doBy)

data("trade.union")
attach(trade.union)

### CONTINGENCY TABLE USING DEFAULT PACKAGES ###

# count workers on each wage
wage_counts <- aggregate(wage, by=list(wage), length)

# count union members on each wage
member_counts <- aggregate(union.member, by=list(wage), sum)

# combine into contingency table
ct <- cbind(member_counts, wage_counts$x)
names(ct) <- c("x", "y", "n")

# check table
head(ct, 10)

### CONTINGENCY TABLE USING DOBY PACKAGE ###

ct <- summaryBy(union.member ~ wage, data = trade.union, FUN=c(sum, length))
names(ct) <- c("x", "y", "n")

# check table
head(ct, 10)

### FIT MODEL ###

# result: coeff estimates and SEs are the same, but the deviances change

# fit glm using grouped data
# cbind(successes, failures) ~ input
# no. union members & no. non-union members, as a function of wage
m.grp <- glm(cbind(y, n - y) ~ x, data = ct, family = "binomial")
summary(m.grp)

# fit glm using ungrouped data
m <- glm(union.member ~ wage, family = "binomial")
summary(m)

### HOSMER-LEMESHOW STATISTIC - USING MATHS

# probability of union membership, given a wage
prob <- function(b0, b1, wage) {
  exp(b0 + b1*wage) / (1 + exp(b0 + b1*wage))
}

# expected number of union members, given group size and probability
exp_freq <- function(n, p) {
  n*p
}

# coefficients of logistic regression model, needed for expected frequencies
b0 <- m.grp$coefficients[1]
b1 <- m.grp$coefficients[2]

# hosmer-lemeshow statistic

exp_freqs <- exp_freq(ct$n, prob(b0, b1, ct$x))
num <- (ct$y - exp_freqs)**2
denom <- exp_freqs * (1 - exp_freqs/ct$n)

hl.stat <- sum(num/denom)
hl.stat

# hypothesis test

alpha <- 0.05
df <- nrow(ct) - 2
hl.crit <- qchisq(1 - alpha, df=df, lower.tail = FALSE)

if (hl.stat > hl.crit) { print("Reject H0") } else { print("Do not reject H0") }


