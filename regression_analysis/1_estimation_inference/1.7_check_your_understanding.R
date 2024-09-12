# 1.7 Check Your Understanding

### FORMULAS ###

# for binomial where n = known, p = unknown = parameter of interest
# take a single y-value, not a vector

wald <- function(y, p, n) {
  (y-n*p)**2 / (n*p*(1-p))
}


deviance <- function(y, p, n) {
  p_hat <- y / n
  2*(y*log(p_hat/p) + (n-y)*log((1-p_hat)/(1-p)))
}

### EXERCISES ###
# use both wald and deviance statistics to run hypothesis tests
# compare the results

alpha <- 0.05
df <- 1 # because each model has only one non-fixed parameter, p
crit_val <- qchisq(1-alpha, df=df)

n <- 10
y <- 3


for (p in c(0.1, 0.3, 0.5)) {
  
  cat("p = ", p, "\n")
  
  w_stat <- wald(y, p, n)
  cat("wald = ", w_stat, "\n")
  
  d_stat <- deviance(y, p, n)
  cat("deviance = ", d_stat, "\n")
  
  if (w_stat < crit_val) { cat("wald: H0 accept\n") } else { cat("wald: H0 reject\n") }
  if (d_stat < crit_val) { cat("deviance: H0 accept\n\n") } else { cat("deviance: H0 reject\n\n") }
  
}



