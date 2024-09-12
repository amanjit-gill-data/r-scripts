# 6.3 GAM FITTING WITH MGCV()
# demo using brain imaging data

library(mgcv) # for gam()
library(gamair) # for brain data

data(brain)
attach(brain)

# 1. REMOVE OUTLIERS

brain <- brain[medFPQ > 5e-3, ]

# 2. FIT GAM AND CHECK MODEL ASSUMPTIONS

m0 <- gam(medFPQ ~ s(Y, X, k=250))
gam.check(m0)

# 3. TRY ALTERNATIVE MODELS
# m1 = 4-th root transformation of response
# m2 = log gamma response
# RHS stays the same for both models

m1 <- gam(medFPQ^.25 ~ s(Y, X, k=250))
m2 <- gam(medFPQ ~ s(Y, X, k=250), family = Gamma(link=log))

gam.check(m1)
gam.check(m2)

# 4. COMPARE MEANS AND CONTOURS

mean(m0$fitted.values)
mean(m1$fitted.values^4)
mean(m2$fitted.values)

# observations
# log-gamma (m2) is close to actual mean; therefore it is approx. unbiased
# 4th-root transformation (m1) is less than actual mean; therefore biased
# therefore log-gamma looks preferable; compare plots to be sure

par(mfrow=c(1, 3))

vis.gam(m0, plot.type="contour", too.far=.03, col="grey", n.grid=60, zlim=c(-1,2), main="GAM with s()")
vis.gam(m1, plot.type="contour", too.far=.03, col="grey", n.grid=60, zlim=c(-1,2), main="GAM with y^.25")
vis.gam(m2, plot.type="contour", too.far=.03, col="grey", n.grid=60, zlim=c(-1,2), main="GAM with gamma response")

# 5. TRY ADDITIVE LOG-GAMMA MODEL AND FEWER DFS

m3 <- gam(medFPQ ~ s(Y, k=30) + s(X, k=30), family = Gamma(link=log))

par(mfrow=c(1, 2))

# compare with chosen log-gamma model
vis.gam(m2, plot.type="contour", too.far=.03, col="grey", n.grid=60, zlim=c(-1,2), main="Chosen GAM: log-gamma")
vis.gam(m3, plot.type="contour", too.far=.03, col="grey", n.grid=60, zlim=c(-1,2), main="Chosen GAM: log-gamma additive")

m2$gcv.ubre
m3$gcv.ubre

AIC(m3, m2)

# observations:
# m3 has extra artefects not supported by data
# m2 has smaller GCV error
# m2 has smaller AIC
# therefore retain chosen GAM, m2

detach(brain)
