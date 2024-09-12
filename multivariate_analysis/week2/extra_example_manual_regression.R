library(dplyr)

pizza <- read.csv("Examples and Exercises/datasets/pizza.csv")

lm.all <- lm(cbind(cal, sodium) ~ prot + fat + carb, data=pizza)
lm.cal <- lm(cal ~ prot + fat + carb, data=pizza)


# manually, using regression equation
# should get same coeffs for cal equation as lm.all and lm.cal

C <- cov(pizza %>% select(prot, fat, carb))

sig0 <- rbind(
    cov(pizza$cal, pizza$prot),
    cov(pizza$cal, pizza$fat),
    cov(pizza$cal, pizza$carb)
)

mu.x <- colMeans(pizza %>% select(-brand))[c("prot", "fat", "carb")]

mu.y <- mean(pizza$cal)

b <- mu.y - t(sig0) %*% solve(C) %*% mu.x

coeffs <- t(sig0) %*% solve(C)

