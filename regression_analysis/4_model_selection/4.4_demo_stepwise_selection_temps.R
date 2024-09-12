# 4.4 STEPWISE VARIABLE SELECTION
# demo on backward and forward selection, using Sydney max temps dataset

mos <- read.table("mos.df.txt", header=TRUE)

# BACKWARD

m <- lm(Maxtemp ~ ., data=mos)

drop1(m)

m <- lm(Maxtemp ~ ., data=mos[,-2])

drop1(m)

# stop here; dropping none is best
# final model is Maxtemp ~ Modsp + Modthik

# FORWARD

m <- lm(Maxtemp ~ 1, data=mos)

add1(m, scope=~Modst+Modsp+Modthik)

m <- lm(Maxtemp ~ Modthik, data=mos)

add1(m, scope=~Modst+Modsp+Modthik)

m <- lm(Maxtemp ~ Modthik+Modsp, data=mos)

add1(m, scope=~Modst+Modsp+Modthik)

# stop here; adding none is best
# final model is Maxtemp ~ Modsp + Modthik
