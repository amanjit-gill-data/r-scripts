# POLYNOMIAL REGRESSION
# demo on fit and plot using Wage dataset

library(ISLR)
data("Wage")
Wage <- Wage[order(Wage$age), ]

pfit <- lm(wage ~ poly(age, 4, raw = TRUE), data=Wage)

# get standard errors to plot confidence band
preds <- predict(pfit, newdata = list(age=Wage$age), se.fit = TRUE)

plot(Wage$age, Wage$wage, cex=0.5, main="Wage wrt Age")
lines(Wage$age, preds$fit, col="blue", lwd=2)
lines(Wage$age, preds$fit + 2*preds$se.fit, lty=4, col="blue")
lines(Wage$age, preds$fit - 2*preds$se.fit, lty=4, col="blue")




