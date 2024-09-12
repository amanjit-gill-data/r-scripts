# WEEK 2 CHALLENGE - PCA

library(dplyr)

pizza <- read.csv("datasets/pizza.csv") %>% 
    filter(brand=="A") %>% 
    select(-brand)

# DO THE PCA

(pizza.pc <- prcomp(pizza, scale=TRUE))

# VIEW RESULTS OF PCA

summary(pizza.pc)

screeplot(pizza.pc)

# SELECT PCs
# METHOD 1: 90% VARIANCE EXPLAINED

(lambdas <- pizza.pc$sdev^2)
(var.explained <- cumsum(lambdas / sum(lambdas)))
min(which(var.explained>=0.9))

# METHOD 2: KAISER'S RULE
max(which(lambdas>=1))

# RESULT: Choose k=2 principal components.


