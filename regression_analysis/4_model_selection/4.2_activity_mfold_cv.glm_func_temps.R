# 4.2 M-FOLD CROSS VALIDATION
# ACTIVITY ON SYDNEY TEMPERATURES DATASET

library(boot)

mos <- read.csv("mos.df.txt", header = TRUE, sep = '')
head(mos)

m <- glm(Maxtemp ~ ., data=mos, family = gaussian())
summary(m)

cv.10.fold <- cv.glm(mos, m, K = 10)
cv.10.fold$delta[1]

