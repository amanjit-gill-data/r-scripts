# 4.3 DEMO ON INFORMATION CRITERIA
# calculate AIC, BIC & adjusted R^2 for different models of cheddar cheese data

library(faraway)
data("cheddar")

# MAKE THE DIFFERENT MODELS

lm.ace <- lm(taste ~ Acetic, data = cheddar)
lm.h2s <- lm(taste ~ H2S, data = cheddar)
lm.lac <- lm(taste ~ Lactic, data = cheddar)
lm.ace.h2s <- lm(taste ~ Acetic + H2S, data = cheddar)
lm.ace.lac <- lm(taste ~ Acetic + Lactic, data = cheddar)
lm.h2s.lac <- lm(taste ~ H2S + Lactic, data = cheddar)
lm.all <- lm(taste ~ ., data = cheddar)

# AIC
aic <- AIC(lm.ace, lm.h2s, lm.lac, lm.ace.h2s, lm.ace.lac, lm.h2s.lac, lm.all)

# BIC
bic <- BIC(lm.ace, lm.h2s, lm.lac, lm.ace.h2s, lm.ace.lac, lm.h2s.lac, lm.all)

# adjusted R^2
adj.rsq <- vector(length=7) # 1 for each model
adj.rsq[1] <- summary(lm.ace)$adj.r.squared
adj.rsq[2] <- summary(lm.h2s)$adj.r.squared
adj.rsq[3] <- summary(lm.lac)$adj.r.squared
adj.rsq[4] <- summary(lm.ace.h2s)$adj.r.squared
adj.rsq[5] <- summary(lm.ace.lac)$adj.r.squared
adj.rsq[6] <- summary(lm.h2s.lac)$adj.r.squared
adj.rsq[7] <- summary(lm.all)$adj.r.squared

# PLOT AIC AND BIC TOGETHER

plot(NULL, xaxt="n", xlab="Model", ylab="Inf. Criterion Value", 
     xlim=c(1,7), ylim=c(225,255))

title("AIC and BIC")

axis(
  side=1, at=1:7,
  labels=c("Acetic", "H2S", "Lactic", "Ace+H2S", "Ace+Lac", "H2S+Lac", "All")
)

points(aic$AIC, col="red", type="b", pch=16)
points(bic$BIC, col="blue", type="b", pch=16)

legend(x = "topright", legend = c("AIC", "BIC"), lty=1, pch=16, col=c("red", "blue"))

# PLOT ADJUSTED R^2 ON ITS OWN

plot(NULL, xaxt="n", xlab="Model", ylab="Inf. Criterion Value", 
     xlim=c(1,7), ylim=c(0.25,0.65))

title("Adjusted R^2")

axis(
  side=1, at=1:7,
  labels=c("Acetic", "H2S", "Lactic", "Ace+H2S", "Ace+Lac", "H2S+Lac", "All")
)

points(adj.rsq, col="red", type="b", pch=16)


