# 5.2 POLYNOMIAL REGRESSION - SELECT DEGREE
# demo using Wage dataset; see activity for neater code

library(ISLR)
library(boot)
data("Wage")

# METHOD 1 - ANOVA
# must use lm() to fit each model, not glm(), so anova() will output p-values

# results
# 1-2 low P therefore 1 is insufficient
# 2-3 low P therefore 2 is insufficient
# 3-4 high P therefore 3 is sufficient and 4 is not much improvement over 3
# therefore choose model 4

fit1 <- lm(wage ~ poly(age, 1, raw=TRUE), data=Wage)
fit2 <- lm(wage ~ poly(age, 2, raw=TRUE), data=Wage)
fit3 <- lm(wage ~ poly(age, 3, raw=TRUE), data=Wage)
fit4 <- lm(wage ~ poly(age, 4, raw=TRUE), data=Wage)
fit5 <- lm(wage ~ poly(age, 5, raw=TRUE), data=Wage)

anova(fit1, fit2, fit3, fit4, fit5)

# METHOD 2 - CROSS VALIDATION
# must use glm() to fit each model, not lm(), so cv.glm() will output non-NaN deltas
# cv.glm() estimates the prediction error for the model, using K folds

# results:
# 1-2 big improvement in error ~ 76
# 2-3 small improvement in error ~ 2 
# 3-4 small improvement in error ~ 3
# 4-5 smaller improvement in error ~ 1
# improvements seem to plateau at model 3 i.e. from model 2 to 3, then onwards
# therefore choose model 3

fit1 <- glm(wage ~ poly(age, 1, raw=TRUE), data=Wage)
fit2 <- glm(wage ~ poly(age, 2, raw=TRUE), data=Wage)
fit3 <- glm(wage ~ poly(age, 3, raw=TRUE), data=Wage)
fit4 <- glm(wage ~ poly(age, 4, raw=TRUE), data=Wage)
fit5 <- glm(wage ~ poly(age, 5, raw=TRUE), data=Wage)

k = 10 # choose 10-fold
set.seed(1) # so the results are always the same
cv.fit1 <- cv.glm(Wage[, c("wage", "age")], fit1, K = k)
cv.fit2 <- cv.glm(Wage[, c("wage", "age")], fit2, K = k)
cv.fit3 <- cv.glm(Wage[, c("wage", "age")], fit3, K = k)
cv.fit4 <- cv.glm(Wage[, c("wage", "age")], fit4, K = k)
cv.fit5 <- cv.glm(Wage[, c("wage", "age")], fit5, K = k)

cv.fit1$delta
cv.fit2$delta
cv.fit3$delta
cv.fit4$delta
cv.fit5$delta

