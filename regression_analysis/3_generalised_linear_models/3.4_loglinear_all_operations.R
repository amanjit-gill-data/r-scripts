# 3.4 Log-Linear Regression - ANOVA Example

library(dobson)
data("melanoma")

# 12 x 3 tibble
# type | site | freq
melanoma

# FOR ALL MODELS, n = no. groups (not the sample size) = 12
n <- nrow(melanoma)

# MODEL 1
# saturated model i.e. no. parameters = sample size
# NOTE: * instead of + due to interaction terms i.e. dependent
m.sat <- glm(frequency ~ site*type, data = melanoma, family = "poisson")
summary(m.sat)

# MODEL 2
# no interaction terms i.e. EVs are independent
# NOTE: + due to the terms *not* interacting
m.ind <- glm(frequency ~ site + type, data = melanoma, family = "poisson")
summary(m.ind)

# MODEL 3 
# minimal model i.e. no coefficients, only the intercept
m.0 <- glm(frequency ~ 1, data = melanoma, family = "poisson")
summary(m.0)

# COMPUTE THE SAME EXP FREQS USING COEFF ESTIMATES

exp_freq <- function (mu, alpha, beta, interaction) {
  exp(mu + alpha + beta + interaction)
}

# model 1: saturated (dependent)

# hutchinson's freckle + extremities
mu <- 2.3026
alpha <- 0
beta <- 0
interaction <- 0
exp_freq(mu, alpha, beta, interaction)

# indeterminate + head&neck
mu <- 2.3026
alpha <- 1.0296 
beta <- 0.7885
interaction <- -1.7228
exp_freq(mu, alpha, beta, interaction)

# nodular + trunk
mu <- 2.3026
alpha <- 1.9879
beta <- -1.6094
interaction <- 0.8155
exp_freq(mu, alpha, beta, interaction)

# model 2: independent

# hutchinson's freckle + extremities
mu <- 2.9554
alpha <- 0
beta <- 0
interaction <- 0
exp_freq(mu, alpha, beta, interaction)

# indeterminate + head&neck
mu <- 2.9554
alpha <- 0.4990 
beta <- -1.2010
interaction <- 0
exp_freq(mu, alpha, beta, interaction)

# nodular + trunk
mu <- 2.9554
alpha <- 1.3020
beta <- -0.7571
interaction <- 0
exp_freq(mu, alpha, beta, interaction)

# model 3: minimal
# all 3 covariate patterns lead to the same exp frequencies
mu <- 3.507 
alpha <- 0
beta <- 0
interaction <- 0
exp_freq(mu, alpha, beta, interaction)

