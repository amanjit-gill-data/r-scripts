# WEEK 4 DEMO - STRUCTURAL EQUATION MODELLING

library(lavaan)
library(tidySEM)

# load covariance matrix

lower <- '
 11.834
  6.947   9.364
  6.819   5.091  12.532
  4.783   5.028   7.495   9.986
 -3.839  -3.889  -3.841  -3.625  9.610
-21.899 -18.831 -21.748 -18.775 35.522 450.288 '

anomie.cov <- getCov(lower, names = c("anomie67", "powerless67", "anomie71", 
              "powerless71", "educ", "sei"))

# specify the SEM

anomie.sem <- '
  # latent variables
    alien67 =~ 1.0*anomie67 + 0.833*powerless67 # 1.
    alien71 =~ 1.0*anomie71 + 0.833*powerless71 # 2.
    ses     =~ 1.0*educ + sei # 3.
  # regressions
    alien67 ~ ses # 4.
    alien71 ~ alien67 + ses # 5.
  # constrained variances
    anomie67 ~~ theta1*anomie67 # 6.
    anomie71 ~~ theta1*anomie71 # 6.
    powerless67 ~~ theta2*powerless67 # 7.
    powerless71 ~~ theta2*powerless71 # 7.
  # correlated residuals
    anomie67 ~~ theta3*anomie71 # 8.
    powerless67 ~~ theta3*powerless71 # 9.
'

# fit the SEM
anomie.fit <- sem(anomie.sem, sample.cov = anomie.cov, sample.nobs = 932)

# visualise the SEM to verify it makes sense
graph_sem(model=anomie.fit, spacing_x=3, rect_height=0.4, ellipse_height=0.5, text_height=2)

# interpret results
summary(anomie.fit, standardized=TRUE)

# RESULTS 

# lack-of-fit test:
# reject hypothesis that the data came from a different model to this

# latent variables:
# higher SES positively affects SEI

# regressions:
# alienation in both 1967 and 1971 is highly correlated to SES
# alienation in 1967 results in alienation in 1971

# covariances:
# after accounting for fixed relationships between alienation and anomie and 
# powerlessless, residual anomie in 1967 is strongly positively correlated with
# anomie in 1971; same can be observed for powerlessness


