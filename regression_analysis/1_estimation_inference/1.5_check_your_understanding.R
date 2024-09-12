# 1.5 Check Your Understanding
# plot binomial, normal and poisson RVs

### METHOD 1 - PLOT() ###

# for sequence of x values:
# make sure they're valid for the distribution i.e. integers if discrete

plot(dbinom(x=seq(0,40), size=40, prob=0.5), type="l")

plot(dnorm(seq(-4, 12, 0.1), mean=4, sd=1.5), type="l")

plot(dpois(x=seq(0,10), lambda=1), pch='*')

### METHOD 2 - CURVE() ###

# expr:     expression wrt to x
# from, to: x interval
# n:        no. x values to plot at

# for binomial (successes/failures):
# you end up specifying the number of trials twice, in 'size' and 'n'
# set n = 1 more than needed, so x = 0 successes and 40 successes are included 
curve(expr=dbinom(x, size=40, p=0.5), from=0, to=40, n=41)

curve(expr=dnorm(x, mean=4, sd=1.5), from=-4, 12, n=100)

# x values must be integers, and there aren't enough to make a nice line
# so plot points inside of lines
curve(expr=dpois(x, lambda=1), from=0, to=10, n=11, type="p")

