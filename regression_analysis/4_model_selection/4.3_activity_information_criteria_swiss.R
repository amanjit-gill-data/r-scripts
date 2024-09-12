# 4.3 INFORMATION CRITERIA
# activity on computing AIC and BIC for swiss dataset

data("swiss")

# FULL MODEL (ALL EVS)

m.full <- lm(Fertility ~ ., data=swiss)
aic.full <- AIC(m.full)
bic.full <- BIC(m.full)

# SIMPLER MODEL (ALL EVS EXCEPT ONE)

m.simpler <- lm(Fertility ~ ., data=swiss[,-3])
aic.simpler <- AIC(m.simpler)
bic.simpler <- BIC(m.simpler)

# aic.simpler < aic.full
aic.full
aic.simpler

# bic.simpler < bic.full
bic.full
bic.simpler

# simpler model has lower AIC and BIC; therefore choose simpler model

