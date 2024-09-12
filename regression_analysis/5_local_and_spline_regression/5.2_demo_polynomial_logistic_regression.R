# 5.2 POLYNOMIAL REGRESSION - POLYNOMIAL LOGISTIC
# demo - fit logistic model to a polynomial using Wage data

library(ISLR)
data("Wage")

Wage <- Wage[order(Wage$age), ] # so the lines in the plot join properly

# fit logistic model by setting family = binomial
# wage>250 turns all the wages into TRUEs and FALSEs
lfit <- glm(I(wage>250) ~ poly(age, 4), data=Wage, family=binomial)

# get predictions with their SEs
# these return the logits, not the assigned classes or their probabilities
preds <- predict(lfit, list(age = Wage$age), se=TRUE)

# compute probabilities from the logits...
probs <- exp(preds$fit) / (1 + exp(preds$fit))

# ...OR get the probabilities directly from predict(type="response")
probs <- predict(lfit, list(age = Wage$age), type="response")

# SE bands for logits
band.logits.l <- preds$fit - 2*preds$se.fit
band.logits.u <- preds$fit + 2*preds$se.fit

# SE bands for probabilities
band.probs.l <- exp(band.logits.l) / (1 + exp(band.logits.l))
band.probs.u <- exp(band.logits.u) / (1 + exp(band.logits.u))

# plot the ages slightly off their correct x-locations, so concentrations of cases 
# at particular ages become more obvious
# also, plot the class 1 cases at 0.2 rather than 1, to shorten the y-scale
# and make the shape of the probability curve more visible
plot(jitter(Wage$age), (Wage$wage>250)/5, pch= "|", col= "darkgrey")

lines(Wage$age, probs, col="blue", lwd=1.5)
lines(Wage$age, band.probs.l, lty=4, col="blue")
lines(Wage$age, band.probs.u, lty=4, col="blue")



