# 3.2 Logistic Regression - Trade Union Example
# basics - plot and fit model


library(SemiPar)

data("trade.union")
attach(trade.union)

### FUNCTION TO PLOT BINARIES ###

plot_binaries <- function(x, y) {
  plot(x, y, pch="|", col="orange", xlab=substitute(x), ylab=substitute(y))
  abline(h = c(0, 1), lty = "dashed")
}

### FIT MODEL - UNION MEMBERSHIP WRT WAGE ###

glm.fit.wage <- glm(union.member ~ wage, family = "binomial")

# plot regression line over the binaries
plot_binaries(wage, union.member)
o.wage <- order(wage)
lines(wage[o.wage], glm.fit.wage$fitted.values[o.wage], col="blue", lwd=2)

### FIT MODEL - UNION MEMBERSHIP WRT AGE ###

glm.fit.age <- glm(union.member ~ age, family = "binomial")

# plot regression line over the binaries
plot_binaries(age, union.member)
o.age <- order(age)
lines(age[o.age], glm.fit.age$fitted.values[o.age], col="red", lwd=2)

### ASSESS SIGNIFICANCE OF EACH PREDICTOR ###

# both wage and age are significant
cat("wage: ", summary(glm.fit.wage)$coefficients[2,])
cat("age:  ", summary(glm.fit.age)$coefficients[2,])

### FIT MODEL - UNION MEMBERSHIP WRT WAGE AND AGE ###

glm.fit <- glm(union.member ~ wage+age, family = "binomial")

### PREDICTION ###

wage <- 6.5
age <- 56

b0 <- glm.fit$coefficients[1]
b1 <- glm.fit$coefficients[2]
b2 <- glm.fit$coefficients[3]

prob <- exp(b0 + b1*wage + b2*age) / (1 + exp(b0 + b1*wage + b2*age))

# now classify the case
threshold <- 0.5
if (prob > threshold) { print("Yes") } else { print("No") }

