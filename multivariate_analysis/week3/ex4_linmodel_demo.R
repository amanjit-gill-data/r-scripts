# WEEK 3 DEMO - MULTIVARIATE LINEAR MODEL AND MANOVA

library(GGally)
library(dplyr)
library(readr)
library(purrr)

# SET UP DATA

'weight waist pulse chins situps jumps
   191  36  50   5  162   60
   189  37  52   2  110   60
   193  38  58  12  101  101
   162  35  62  12  105   37
   189  35  46  13  155   58
   182  36  56   4  101   42
   211  38  56   8  101   38
   167  34  60   6  125   40
   176  31  74  15  200   40
   154  33  56  17  251  250
   169  34  50  17  120   38
   166  33  52  13  210  115
   154  34  64  14  215  105
   247  46  50   1   50   50
   193  36  46   6   70   31
   202  37  62  12  210  120
   176  37  54   4   60   25
   157  32  52  11  230   80
   156  33  54  15  225   73
   138  33  68   2  110   43' %>%
    textConnection() %>% read.table(header=TRUE) -> fitness

summary(fitness)
ggpairs(fitness)

# MULTIVARIATE LINEAR MODEL

# Results:
# for chins, only waist is significant
# for situps, only waist is significant
# for jumps, none are significant

fitness.lm <- lm(cbind(chins, situps, jumps) ~ waist+pulse+weight, data=fitness)

summary(fitness.lm)

# bring coefficients for all 3 RVs into one p x k matrix
coef(fitness.lm)

# create a variance-covariance matrix
vcov(fitness.lm)

# estimate variance between RVs
estVar(fitness.lm)

# calculate residuals for all RV observations
resid(fitness.lm)

# fitted RV values for all observations
fitted(fitness.lm)

# MANOVA - HYPOTHESIS TESTING

# Results:
# Using these multivariate tests (rather than testing each
# RV individually) confirms the earlier result that waist is
# the only significant factor.

anova(fitness.lm) # Pillai by default
anova(fitness.lm, test="Wilks")
anova(fitness.lm, test="Hotelling-Lawley")
anova(fitness.lm, test="Roy")

# TEST EFFECT OF WEIGHT BY REDOING LINEAR MODEL WITH THE
# OTHER 2 RVs

fitness.lm2 <- lm(cbind(chins,situps,jumps)~waist+pulse, data=fitness)
summary(fitness.lm2)

# can compare the two models!
anova(fitness.lm, fitness.lm2)

# DIAGNOSTICS 

# plot residuals of first model
ggpairs(as_tibble(resid(fitness.lm)))

# can calculate Pearson residuals manually
sds <- fitness.lm %>% estVar %>% diag %>% sqrt
PR <- sweep(resid(fitness.lm), 2, sds, '/')
ggpairs(as_tibble(PR))

# decorrelate residuals
# close to identity matrix
Uinv <- estVar(fitness.lm) %>% chol %>% solve
(PRI <- resid(fitness.lm)%*%Uinv) %>% cor %>% zapsmall
ggpairs(as_tibble(PRI))

# residuals vs fitted
pairs(as_tibble(cbind(PR,fitted(fitness.lm))), horInd=1:3,verInd=4:6,
      panel=function(x,y,...){abline(h=0,col="gray");points(x[abs(y)<2],y[abs(y)<2]);if(any(abs(y)>=2)) text(x[abs(y)>=2],y[abs(y)>=2],labels=which(abs(y)>=2));lines(lowess(x,y),col="red")})




