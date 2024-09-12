# 6.1 GAM - ANOVA TO COMPARE MODELS
# activity using Wages data

library(ISLR)
library(gam)

data(Wage)
attach(Wage)

# compare 3 GAMs
# 1. exclude year
# 2. linear function for year
# 3. spline function for year
# keep age and education same as in demos

m1 <- gam(wage ~ s(age, df=5) + education)
m2 <- gam(wage ~ year + s(age, df=5) + education)
m3 <- gam(wage ~ s(year, df=4) + s(age, df=5) + education)

anova(m1, m2, m3, test="F")

# anova results:
# m2 is significantly better than m1
# but m3 is not significantly better than m2
# so year should be modelled as linear, as in m2


