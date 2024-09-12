# 4.4 STEPWISE VARIABLE SELECTION
# demo on backward and forward selection, using cheddar dataset
# keep looking for lowest AIC

# BACKWARD ELIMINATION

library(faraway)
data("cheddar")

# first fit the full model
m <- lm(taste ~ ., data = cheddar)

# execute drop1() to compare which one to drop
drop1(m)

# removing Acetic yields lowest AIC, so remove it
m <- lm(taste ~ H2S+Lactic, data = cheddar)

# execute drop1() to compare which one to drop
drop1(m)

# removing none yields the lowest AIC, so stop here

# FORWARD SELECTION

# first fit the minimal model
m <- lm(taste ~ 1, data = cheddar)

# execute add1() to compare which one to add
# specify all three predictors in the scope for consideration
add1(m, scope=~Acetic+H2S+Lactic)

# adding H2S would yield smallest AIC, so add it to the model
m <- lm(taste ~ H2S, data = cheddar)

# execute add1() to compare which one to add
# need to specify all 3 predictors in the scope again
add1(m, scope=~Acetic+H2S+Lactic)

# adding Lactic would yield lowest AIC, so add it to the model
m <- lm(taste ~ H2S+Lactic, data = cheddar)

# execute add1() to compare which one to add
# need to specify all 3 predictors in the scope again
add1(m, scope=~Acetic+H2S+Lactic)

# adding none yields the lowest AIC, so stop here

