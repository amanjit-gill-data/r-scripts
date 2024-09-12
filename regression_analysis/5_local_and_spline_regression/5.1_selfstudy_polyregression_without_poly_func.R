# 5.1 POLYNOMIAL REGRESSION
# self study - enter polynomial formula into lm() without the help of poly()

eth <- read.table("ethanol.txt", header=TRUE)

# order the predictors so the lines joining the fitted values won't 
# criss-cross the plot
eth <- eth[order(eth$E), ]

plot(eth$NOx ~ eth$E, main="Ethanol", cex=.75)

# WITH AND WITHOUT POLY()

E.sq <- eth$E^2

polyfit1 <- lm(eth$NOx ~ poly(eth$E, 2, raw=TRUE))
polyfit2 <- lm(eth$NOx ~ eth$E + E.sq) # squaring has to be done before lm()

