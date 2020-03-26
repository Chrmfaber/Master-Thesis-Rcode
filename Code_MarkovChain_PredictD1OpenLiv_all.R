#################################
# Back transformation functions
# (Normalized and Scaled)
#################################
bt <- function(yt){
  y.bt <- (exp(yt)/(exp(yt)+1))
  return(list(y.l=y.bt))
}

#####################
M1 <- Model_O;M1
dat <- dat[dat$WindowClosed==1,]
summary(dat$TempC) #summary(dat[dat$WindowClosed==1,]$TempC)
summary(dat$CO2C_log)
summary(dat$SolRad_log)
summary(dat$OutdoorTemp)
par(mfrow=c(2,2))
pred <- predict(M1,se=TRUE,
                newdata=data.frame(TempC=seq(18.5,27,0.1), CO2C_log=mean(dat$CO2C_log), SolRad_log=mean(dat$SolRad_log), OutdoorTemp=mean(dat$OutdoorTemp)
                ),interval="prediction")
pred1 <- cbind(pred$fit,pred$fit-1.96*pred$se.fit,pred$fit+1.96*pred$se.fit)
pred <- predict(M1,se=TRUE,
                 newdata=data.frame(TempC=mean(dat$TempC), CO2C_log=seq(5.5,7.5,0.1), SolRad_log=mean(dat$SolRad_log), OutdoorTemp=mean(dat$OutdoorTemp)
                 ),interval="prediction")
pred2 <- cbind(pred$fit,pred$fit-1.96*pred$se.fit,pred$fit+1.96*pred$se.fit)
pred <- predict(M1,se=TRUE,
                 newdata=data.frame(TempC=mean(dat$TempC), CO2C_log=mean(dat$CO2C_log), SolRad_log=seq(-0.2,7,0.1), OutdoorTemp=mean(dat$OutdoorTemp)
                 ),interval="prediction")
pred3 <- cbind(pred$fit,pred$fit-1.96*pred$se.fit,pred$fit+1.96*pred$se.fit)
pred <- predict(M1,se=TRUE,
                 newdata=data.frame(TempC=mean(dat$TempC), CO2C_log=mean(dat$CO2C_log), SolRad_log=mean(dat$SolRad_log), OutdoorTemp=seq(-5.5,24.5,0.1)
                 ),interval="prediction")
pred4 <- cbind(pred$fit,pred$fit-1.96*pred$se.fit,pred$fit+1.96*pred$se.fit)

###########################################
## Presenting the result in transformation
###########################################
matplot(seq(18.5,27,0.1),pred1,type="l",col=c(1,2,2),
        lty=c(1,2,2),lwd = 2,xlab = "Indoor temperature",
        main=paste("Prediction in transformed domain"),ylab = "Window opening probability")
grid();rug(dat$TempC)
matplot(seq(5.5,7.5,0.1),pred2,type="l",col=c(1,2,2),
        lty=c(1,2,2),lwd = 2,xlab = "CO2 concentration",
        main=paste("Prediction in transformed domain"),ylab = "Window opening probability")
grid();rug(dat$CO2C_log)
matplot(seq(-0.2,7,0.1),pred3,type="l",col=c(1,2,2),
        lty=c(1,2,2),lwd = 2,xlab = "Solar radiation",
        main=paste("Prediction in transformed domain"),ylab = "Window opening probability")
grid();rug(dat$SolRad_log)
matplot(seq(-5.5,24.5,0.1),pred4,type="l",col=c(1,2,2),
        lty=c(1,2,2),lwd = 2,xlab = "Outdoor temperature",
        main=paste("Prediction in transformed domain"),ylab = "Window opening probability")
grid();rug(dat$OutdoorTemp)

############################################
## Presenting the result in original domain
############################################
matplot(seq(18.5,27,0.1),bt(pred1)$y.l,type="l",col=c(1,2,2),
        lty=c(1,2,2),lwd = 2,xlab = "Indoor temperature",
        main=paste("Prediction in real domain"),ylab = "Window opening probability")
grid();rug(dat$TempC)
matplot(seq(5.5,7.5,0.1),bt(pred2)$y.l,type="l",col=c(1,2,2),
        lty=c(1,2,2),lwd = 2,xlab = "CO2 concentration",
        main=paste("Prediction in real domain"),ylab = "Window opening probability")
grid();rug(dat$CO2C_log)
matplot(seq(-0.2,7,0.1),bt(pred3)$y.l,type="l",col=c(1,2,2),
        lty=c(1,2,2),lwd = 2,xlab = "Solar radiation",
        main=paste("Prediction in real domain"),ylab = "Window opening probability")
grid();rug(dat$SolRad_log)
matplot(seq(-5.5,24.5,0.1),bt(pred4)$y.l,type="l",col=c(1,2,2),
        lty=c(1,2,2),lwd = 2,xlab = "Outdoor temperature",
        main=paste("Prediction in real domain"),ylab = "Window opening probability")
grid();rug(dat$OutdoorTemp)

############################################
## Presenting variables in original domain
############################################
matplot(exp(seq(5.5,7.5,0.1)),pred2,type="l",col=c(1,2,2),
        lty=c(1,2,2),lwd = 2,xlab = "CO2 concentration",
        main=paste("Prediction in transformed domain"),ylab = "Window opening probability")
grid();rug(dat$CO2C)
matplot(exp(seq(-0.2,7,0.1)),pred3,type="l",col=c(1,2,2),
        lty=c(1,2,2),lwd = 2,xlab = "Solar radiation",
        main=paste("Prediction in transformed domain"),ylab = "Window opening probability")
grid();rug(dat$SolRad)

matplot(exp(seq(5.5,7.5,0.1)),bt(pred2)$y.l,type="l",col=c(1,2,2),
        lty=c(1,2,2),lwd = 2,xlab = "CO2 concentration",
        main=paste("Prediction in real domain"),ylab = "Window opening probability")
grid();rug(dat$CO2C)
matplot(exp(seq(-0.2,7,0.1)),bt(pred3)$y.l,type="l",col=c(1,2,2),
        lty=c(1,2,2),lwd = 2,xlab = "Solar radiation",
        main=paste("Prediction in real domain"),ylab = "Window opening probability")
grid();rug(dat$SolRad)