# 6.1 GENERALISED ADDITIVE MODEL - INTRO
# demo using Wage dataset

library(ISLR) # for Wage data
library(splines) # for ns(), etc
library(gam) # for plot.Gam() and gam()
library(akima) # for plotting local regression with interaction

data(Wage)
attach(Wage)

# NATURAL SPLINES - CAN USE LM()

# fit natural splines to year and age, and linear to education
ns.gam <- lm(wage ~ ns(year, df=4) + ns(age, df=5) + education)

# can plot all effects at once using plot.Gam(), even if model is lm() not gam()
plot.Gam(ns.gam, se=TRUE, main="Natural Splines")

# SMOOTHING SPLINES - MUST USE GAM()

ss.gam <- gam(wage ~ s(year, df=4) + s(age, df=5) + education)

plot.Gam(ss.gam, se=TRUE, main="Smoothing Splines")

# ANOVA TO TEST LINEAR/NON-LINEAR EFFECTS - MUST USE GAM() OBJECT

summary(ss.gam)

# results
# year:     non-linear effect is not significant
#           linear effect is significant
#           therefore use year rather than s(year)
# age:      non-linear effect is significant
#           therefore leave as s(age)
# educ:     already modelled as linear, not non-linear
#           linear effect is significant

# final formula
# wage ~ year + s(age) + education

# LOCAL REGRESSION

lo.gam <- gam(wage ~ s(year, df=4) + lo(age, span=0.7) + education)

plot.Gam(lo.gam, se=TRUE, main="Local Regression")

# LOCAL REGRESSION FOR INTERACTION BETWEEN VARIABLES

lo.int.gam <- gam(wage ~ lo(year, age, span=0.5) + education)

# plot.Gam() won't work for this;
# use plot(), as long as akima and interp are installed
plot(lo.int.gam, se=TRUE)


