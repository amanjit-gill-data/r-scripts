# WEEK 4 CHALLENGE - FACTOR ANALYSIS

library(dplyr)
library(readr)
library(psych)

pizza <- read.csv("datasets/pizza.csv") %>% filter(brand=="A") %>% select(-brand)

# test number of factors
factanal(pizza, 1) # result: 1 factor not sufficient
factanal(pizza, 2) # result: 2 factors not sufficient
factanal(pizza, 3) # result: 3 factors ARE sufficient
factanal(pizza, 4) # result: error, too many factors

# use scree plot to confirm number of factors
fa.parallel(pizza, fm="ml")

# HYP TESTS SAY: 2 are not sufficient
# SCREE PLOT SAYS: 2 are sufficient

# RESULT (from 2-factor analysis)
# factor 1 = fat and calories, modest negative carbs i.e. cheese
# factor 2 = protein, modest calories i.e. meat


