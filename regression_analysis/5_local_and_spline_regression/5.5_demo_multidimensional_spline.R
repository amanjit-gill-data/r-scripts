# 5.5 MULTIDIMENSIONAL SPLINE 
# demo and activity using galaxy data

library(mgcv)
library(lattice)

galaxy <- read.table("galaxy.data", header=TRUE, sep=",")
attach(galaxy)

# METHOD 1: 3-DIMENSIONAL SMOOTHING SPLINE USING MGCV.GAM() WITH S()

fit1 <- gam(velocity ~ s(east.west, north.south)) # s() sets up a smoothing spline

plot(fit1)

# METHOD 2: 3-DIMENSIONAL LOESS SMOOTH USING LOESS.SMOOTH()

fit2 <- loess(velocity ~ north.south + east.west, span = 0.7,
                     degree = 2, normalize = TRUE, family = "gaussian")

x <- seq(min(north.south), max(north.south), length.out = 50)
y <- seq(min(east.west), max(east.west), length.out = 50)

xs <- rep(x, 50)
ys <- vector()

for (i in 1:50) {
  ys <- c(ys, rep(y[i], 50))
}

xy <- data.frame(north.south=xs, east.west=ys)
z <- predict(fit2, newdata=xy)

z <- matrix(z, nrow=50)

wireframe(z, screen = list(z = 170, x = -60))

# solution code

loc.lin.reg = loess(formula = velocity ~ north.south +
                      east.west, data = galaxy, span = 0.7 , degree = 2 ,
                    normalize = T, family = "gaussian" )
attach(galaxy)

xgrid = seq(min(north.south), max(north.south), length.out = 50 )
ygrid = seq(min(east.west), max(east.west), length.out = 50 )
new.coords = matrix(data = NA , nrow = 2500 , ncol = 2 )
new.coords[, 1 ] = as.vector(outer(rep( 1 , 50 ), xgrid))
new.coords[, 2 ] = as.vector(outer(ygrid, rep( 1 , 50 )))
new.coords = as.data.frame(new.coords)
names(new.coords) <- c( "north.south" , "east.west" )
Z = predict(loc.lin.reg, newdata = new.coords)
Z = matrix(data = Z, ncol = 50 )
library(lattice)
wireframe(Z, screen = list(z = 170 , x = - 60 ))

# ACTIVITY - CREATE 3-DIMENSIONAL POINT CLOUD

cloud(velocity ~ east.west + north.south, pretty = T,
      screen = list(z = 170, x = -60))


