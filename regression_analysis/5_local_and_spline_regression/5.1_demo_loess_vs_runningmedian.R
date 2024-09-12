# 5.1 LOESS SMOOTHER VS RUNNING MEDIAN
# demo on diabetes data

diab <- read.table("diabetes.dat", header=TRUE)
diab <- diab[order(diab$Age), ] # smoother assumes predictors are in order

# now do the loess fit
lfit <- loess(diab$C.Peptide ~ diab$Age)
pred <- predict(lfit, diab$Age, se=TRUE) # obtain SEs to plot conf intervals

# plot fitted values +/- 2*SE
plot(diab$Age, diab$C.Peptide, main="Diabetes")
lines(diab$Age, pred$fit, lwd=2)
lines(diab$Age, pred$fit + 2*pred$se.fit, lty=2)
lines(diab$Age, pred$fit - 2*pred$se.fit, lty=2)

# now compute the running median and add to plot
rmed <- runmed(diab$C.Peptide, k=7)
lines(diab$Age, rmed, col="magenta", lwd=1.5)

legend("topleft", legend = c("loess", "running 7-median"),
       lwd=c(2, 1.5), col=c("black", "magenta"), cex=0.75, bty="n")

