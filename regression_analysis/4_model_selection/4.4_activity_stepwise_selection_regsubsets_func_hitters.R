# 4.4 STEPWISE SELECTION USING REGSUBSETS()
# activity on using regsubsets rather than add/drop, using hitters dataset

library(ISLR)
library(leaps)
data("Hitters")

# CLEAN DATA 
sum(is.na(Hitters$Salary))
Hitters <- na.omit(Hitters)
dim(Hitters)

# BACKWARD

regfit.bw <- regsubsets(Salary ~ ., data = Hitters, method = "backward")
summary(regfit.bw)
coef(regfit.bw, 7)

# best 7-variable model is
# Salary ~ AtBat + Hits + Walks + CRuns + CWalks + DivisionW + PutOuts

# FORWARD

regfit.fw <- regsubsets(Salary ~ ., data = Hitters, method = "forward")
summary(regfit.fw)
coef(regfit.fw, 7)

# best 7-variable model is
# Salary ~ AtBat + Hits + Walks + CRBI + CWalks + DivisionW + PutOuts



