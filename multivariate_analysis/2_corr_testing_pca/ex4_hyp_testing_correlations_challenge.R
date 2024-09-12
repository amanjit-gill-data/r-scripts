# WEEK 2 CHALLENGE - HYPOTHESIS TESTING CORRELATIONS

library(GGally)
library(ggm)

red <- read_csv("../datasets/winequality-red.extract7.csv") %>% mutate(
    `log sulphates`=log(`sulphates`), 
    `root4 free sulfur dioxide` = (`free sulfur dioxide`)^(1/4), 
    `root4 total sulfur dioxide` = (`total sulfur dioxide`)^(1/4), 
    `log fixed acidity`=log(`fixed acidity`), 
    `log volatile acidity`=log(`volatile acidity`), 
    .keep="unused")

ggpairs(red)

# TASK 1
# TEST CORRELATION BETWEEN pH and LOG VOLATILE ACIDITY
# result: p-value <<< 0.05, so reject h0
# there is a correlation

(r27 <- cor(red$pH, red$`log volatile acidity`))

pcor.test(r27, 0, nrow(red))

# TASK 2
# TEST PARTIAL CORRELATION BETWEEN pH and LOG VOLATILE ACIDITY,
# GIVEN LOG FIXED ACIDITY
# result: p-value > 0.05, so do NOT reject h0
# there is no partial correlation, as most of correlation is
# due to fixed acidity, not volatile acidity

(r27.6 <- pcor(c(2,7,6), cor(red)))

pcor.test(r27.6, 1, nrow(red))

