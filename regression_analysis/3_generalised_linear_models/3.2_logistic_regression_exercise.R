library("dobson")
library("ResourceSelection")

data("senility")
attach(senility)

head(senility)

# Q1 group data, fit model, obtain coeffs and SEs

# contingency table
score_counts <- aggregate(x, by=list(x), length)
senile_counts <- aggregate(s, by=list(x), sum)
ct <- cbind(senile_counts, score_counts$x)
names(ct) <- c("score", "senile", "total")
head(ct)

# fit model
m1 <-  glm(cbind(senile, total-senile) ~ score, data = ct, family = "binomial")

# coefficients and their standard errors
summary(m1)$coefficients

# Q2 hypothesis test: H0 is that the model is *correct*

# pearson chi-squared and deviance
psq <- sum(residuals.glm(m1, type = "pearson")**2)
D <- m1$deviance

# critical chi-squared value
alpha <- 0.05
N <- nrow(ct)
p <- length(m1$coefficients)
chisq.crit <- qchisq(alpha, df=N-p, lower.tail = FALSE)

cat("P^2 Test: Reject = ", psq > chisq.crit)
cat("D Test: Reject = ", D > chisq.crit)

# result: do not reject H0 i.e. we think the model is correct

# Q3 produce table with x, y, n, pi.hat, p.resid, d.resid

x <- ct$score
y <- ct$senile
n <- ct$total

# for pi.hat, can just get the fitted values
pi.hat <- m1$fitted.values

# or can calculate them using formula
prob <- function(b0, b1, score) {
  exp(b0 + b1*score) / (1 + exp(b0 + b1*score))
}
b0 <- m1$coefficients[1]
b1 <- m1$coefficients[2]
pi.hat <- prob(b0, b1, x)

# pearson resids
p.resid <- residuals(m1, type = "pearson")

# deviance resids
d.resid <- residuals(m1, type="deviance")

dobson.table <- cbind(x, y, n, pi.hat, p.resid, d.resid)
dobson.table

# Q4 plot standardised resids against EV

std.p.resid <- p.resid / sqrt(1 - hatvalues(m1))
std.d.resid <- d.resid / sqrt(1 - hatvalues(m1))

plot(ct$score, std.p.resid)
abline(h = c(-2, 0, 2))

plot(ct$score, std.d.resid)
abline(h = c(-2, 0, 2))

# result:
# no discernable pattern; therefore linearity assumption is justified
# all residuals are less than 2 SDs from mean

# Q5 compute H-L statistic for g = 3

# re-fit model with no grouping, to use in hoslem.test()
m <- glm(s ~ x, data=senility, family = "binomial")
hl.test <- hoslem.test(s, m$fitted.values, g=3)

hl.test$expected
hl.test$observed
hl.test$statistic
hl.test$p.value

# result:
# p > alpha i.e. outside of the 5% rejection region
# therefore we cannot reject H0 i.e. it's a good fit

# how the solution worded it:
# the null hypothesis that the observed and expected proportions are the 
# same across all doses cannot be rejected



