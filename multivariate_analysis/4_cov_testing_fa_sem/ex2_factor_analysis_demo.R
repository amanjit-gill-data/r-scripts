# WEEK 4 DEMO - FACTOR ANALYSIS

library(dplyr)
library(readr)
library(psych)

sec <- read.table(
    file = "datasets/factor.dat", 
    col.names = c("pop", "school", "employ", "services", "house"))

summary(sec)

# NORMAL FACTOR ANALYSIS

# start with 1 factor
(f1 <- factanal(sec, 1))

# hypothesis that 1 factor is sufficient is not accepted
# now try 2 factors
(f2 <- factanal(sec, 2))

# hypothesis that 2 factors are sufficient is accepted
# trying 3 factors causes an error
factanal(sec, 3)

# FACTOR ANALYSIS WITH VARIMAX ROTATION

# 1 factor
(f1.vm <- fa(sec, 1, fm="ml", rotate="varimax"))
plot(f1.vm)

# 2 factors
(f2.vm <- fa(sec, 2, fm="ml", rotate="varimax"))
plot(f2.vm)

# 3 factors
# this will show that we've run out of degrees of freedom
(f3.vm <- fa(sec, 3, fm="ml", rotate="varimax"))
plot(f3.vm)

# SCREE PLOT TO CHOOSE NUMBER OF FACTORS

# result: choose 2 factors
fa.parallel(sec, fm="ml")

# EXTRACTING FA RESULTS FROM FA OBJECTS

unclass(f2)
unclass(f2.vm)


