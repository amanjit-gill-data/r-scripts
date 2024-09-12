# WEEK 2 DEMO - PARTIAL CORRELATIONS

# var1 = intelligence
# var2 = weight
# var3 = age

library(ggm)

R <- matrix(
    c(1, .6162, .8267, .6162, 1, .7321, .8267, .7321, 1),
    3, 3
)

# compute partial correlations for each pair of variables, given the third
parcor(R)

# take just the correlation between var1 and var2, given var3
parcor(R)[1,2]

# do the same thing, using pcor(..) instead
pcor(c(1:2, 3), R)

# result is 0.02862971
# therefore virtually no correlation between intelligence and weight