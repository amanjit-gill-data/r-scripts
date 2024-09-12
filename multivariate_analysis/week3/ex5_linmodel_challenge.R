# WEEK 3 CHALLENGE - MULTIVARIATE LINEAR MODEL AND MANOVA

library(GGally)
library(dplyr)
library(readr)
library(purrr)

select <- dplyr::select

pizza <- read.csv("../datasets/pizza.csv") %>% 
    filter(brand=="A") %>% select(-brand)

summary(pizza)

# TASK1: FIT AND DIAGNOSE MULTIVARIATE LM 
# response vars = cal, ash, sodium
# explanatory vars = fat, prot, carb

# results:
# for cal, all 3 EVs are significant
# for ash, none are significant
# for sodium, none are significant

pizza.lm <- lm(cbind(cal, ash, sodium) ~ fat+prot+carb, data=pizza)
summary(pizza.lm)


# TASK2: TEST WHETHER CARB IS PREDICTIVE OF THE 
# 3 RVs (JOINTLY) IN PRESENCE OF THE OTHER EVs

# Result:
# carb is significant

pizza.lm2 <- lm(cbind(cal, ash, sodium) ~ fat+prot, data=pizza)

summary(pizza.lm2)

anova(pizza.lm, pizza.lm2) # pillai by default

# TASK3: TEST WHETHER FAT IS PREDICTIVE OF THE 
# 3 RVs (JOINTLY) IN PRESENCE OF THE OTHER EVs

pizza.lm3 <- lm(cbind(cal, ash, sodium) ~ prot+carb, data=pizza)

summary(pizza.lm3)

anova(pizza.lm, pizza.lm3)

# Result:
# fat is significant

# TASK4: RELOAD DATASET WITH ALL PIZZA BRANDS. USE MANOVA 
# TO TEST WHETHER DIFFERENT BRANDS HAVE DIFFERENT MEASURED
# PARAMETERS.

# Results:
# for all RVs, brand is highly significant

pizza <- read.csv("../datasets/pizza.csv")

pizza.lm4 <- lm(cbind(cal,ash,sodium,fat,carb,prot)~brand, data=pizza)
summary(pizza.lm4)

pizza.lm4reduced <- lm(cbind(cal,ash,sodium,fat,carb,prot)~1, data=pizza)
anova(pizza.lm4, pizza.lm4reduced)

