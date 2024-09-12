# WEEK 2 CHALLENGE - PARTIAL CORRELATIONS

library(dplyr)
library(readr)
library(GGally)
library(ggm)

# read in data and transform variables

red <- read_csv("../datasets/winequality-red.extract7.csv") %>% mutate(
    `log sulphates`=log(`sulphates`),
    `root4 free sulfur dioxide`=(`free sulfur dioxide`)^(1/4),
    `root4 total sulfur dioxide`=(`total sulfur dioxide`)^(1/4),
    `log fixed acidity`=log(`fixed acidity`),
    `log volatile acidity`=log(`volatile acidity`),
    .keep='unused')

ggpairs(red)

# TASK 1A
# correlation between pH and log volatile acidity

cor(red$pH, red$`log volatile acidity`)

# TASK 1b
# partial correlation between pH and log volatile acidity, 
# given log fixed acidity
# first reduce data matrix to just the 3 relevant columns
# then take the correlation matrix and compute the partial

red.3cols <- red[ ,c(2,6,7)]

R <- cor(red.3cols)
parcor(R)[1,3]

# result: 
# correlation is 0.2191991
# partial correlation, given log fixed acidity, is 0.04213257
# this means that there is a very small correlation between 
# pH and log volatile acidity, but when log fixed acidity is 
# accounted for, the correlation becomes even tinier, suggesting
# that the main interaction is between pH and log fixed acidity, 
# not pH and log volatile acidity.

# TASK 2
# partial correlation between density and log sulphates,
# given free and total sulphur dioxide
# first reduce data matrix to just the 4 relevant columns
# then take the correlation matrix and compute the partial

red.4cols <- red[ ,c(1,3,4,5)]

R <- cor(red.4cols)
parcor(R)[1,2]

# result:
# partial correlation is 0.170947

