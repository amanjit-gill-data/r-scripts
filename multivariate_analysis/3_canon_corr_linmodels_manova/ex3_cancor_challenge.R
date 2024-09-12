# WEEK 3 CHALLENGE - CANONICAL CORRELATIONS

library(CCA)
library(CCP)
library(GGally)
library(dplyr)
library(purrr)

select <- dplyr::select

pizza <- read.csv("../datasets/pizza.csv") %>% 
    filter(brand=="A") %>% 
    select(-brand)

X <- pizza %>% select(cal, ash, sodium)
Y <- pizza %>% select(fat, prot, carb)

# TASK 1: Calculate canonical correlations between the two sets.

pizza.cc <- cc(X,Y)

pizza.cc[c("cor", "xcoef", "ycoef")]

(plt.cc(pizza.cc, var.label = TRUE))

# TASK 2: Test significance between the two sets.
# Result: Only the first canonical correlation is significant.

n <- nrow(pizza)
p <- ncol(X)
q <- ncol(Y)

p.asym(pizza.cc$cor, n, p, q, tstat="Wilks")
p.asym(pizza.cc$cor, n, p, q, tstat="Hotelling")
p.asym(pizza.cc$cor, n, p, q, tstat="Pillai")
p.asym(pizza.cc$cor, n, p, q, tstat="Roy")
