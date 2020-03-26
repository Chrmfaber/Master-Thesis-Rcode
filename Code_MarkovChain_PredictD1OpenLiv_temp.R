#################################
# Back transformation functions
# (Normalized and Scaled)
#################################
bt <- function(yt){
  y.bt <- (exp(yt)/(exp(yt)+1))
  return(list(y.l=y.bt))
}

##################### temp ~ changing CO2
M1 <- Model_O;M1
dat <- dat[dat$WindowClosed==1,]
summary(dat$TempC) #summary(dat[dat$WindowClosed==1,]$TempC)
summary(dat$CO2C_log)
par(mfrow=c(3,2))
pred <- predict(M1,se=TRUE,
                newdata=data.frame(TempC=seq(18.5,27,0.1), CO2C_log=mean(dat$CO2C_log), SolRad_log=mean(dat$SolRad_log), OutdoorTemp=mean(dat$OutdoorTemp)
                ),interval="prediction")
predmean <- cbind(pred$fit,pred$fit-1.96*pred$se.fit,pred$fit+1.96*pred$se.fit)
pred <- predict(M1,se=TRUE,
                newdata=data.frame(TempC=seq(18.5,27,0.1), CO2C_log=6, SolRad_log=mean(dat$SolRad_log), OutdoorTemp=mean(dat$OutdoorTemp)
                ),interval="prediction")
predlow <- cbind(pred$fit,pred$fit-1.96*pred$se.fit,pred$fit+1.96*pred$se.fit)
pred <- predict(M1,se=TRUE,
                newdata=data.frame(TempC=seq(18.5,27,0.1), CO2C_log=7, SolRad_log=mean(dat$SolRad_log), OutdoorTemp=mean(dat$OutdoorTemp)
                ),interval="prediction")
predhigh <- cbind(pred$fit,pred$fit-1.96*pred$se.fit,pred$fit+1.96*pred$se.fit)

###########################################
## Presenting the result in transformation
###########################################
matplot(seq(18.5,27,0.1),predlow,type="l",col=c(1,2,2),
        lty=c(1,2,2),lwd = 2,xlab = "Indoor temperature",
        main=paste("Prediction in transformed domain with changing CO2"),ylab = "Window opening probability")
matlines(seq(18.5,27,0.1),predmean,type="l",col=c(1,3,3),
         lty=c(1,2,2),lwd = 2)
matlines(seq(18.5,27,0.1),predhigh,type="l",col=c(1,4,4),
         lty=c(1,2,2),lwd = 2)
grid();rug(dat$TempC)

############################################
## Presenting the result in original domain
############################################
matplot(seq(18.5,27,0.1),bt(predlow)$y.l,type="l",col=c(1,2,2),
        lty=c(1,2,2),lwd = 2,xlab = "Indoor temperature",ylim = c(0,1),
        main=paste("Prediction in transformed real domain with changing CO2"),ylab = "Window opening probability")
matlines(seq(18.5,27,0.1),bt(predmean)$y.l,type="l",col=c(1,3,3),
         lty=c(1,2,2),lwd = 2)
matlines(seq(18.5,27,0.1),bt(predhigh)$y.l,type="l",col=c(1,4,4),
         lty=c(1,2,2),lwd = 2)
grid();rug(dat$TempC)

##################### temp ~ changing SolRad_log
M1 <- Model_O;M1
dat <- dat[dat$WindowClosed==1,]
summary(dat$TempC) #summary(dat[dat$WindowClosed==1,]$TempC)
summary(dat$SolRad_log)
pred <- predict(M1,se=TRUE,
                newdata=data.frame(TempC=seq(18.5,27,0.1), CO2C_log=mean(dat$CO2C_log), SolRad_log=mean(dat$SolRad_log), OutdoorTemp=mean(dat$OutdoorTemp)
                ),interval="prediction")
predmean <- cbind(pred$fit,pred$fit-1.96*pred$se.fit,pred$fit+1.96*pred$se.fit)
pred <- predict(M1,se=TRUE,
                newdata=data.frame(TempC=seq(18.5,27,0.1), CO2C_log=mean(dat$CO2C_log), SolRad_log=1, OutdoorTemp=mean(dat$OutdoorTemp)
                ),interval="prediction")
predlow <- cbind(pred$fit,pred$fit-1.96*pred$se.fit,pred$fit+1.96*pred$se.fit)
pred <- predict(M1,se=TRUE,
                newdata=data.frame(TempC=seq(18.5,27,0.1), CO2C_log=mean(dat$CO2C_log), SolRad_log=6.5, OutdoorTemp=mean(dat$OutdoorTemp)
                ),interval="prediction")
predhigh <- cbind(pred$fit,pred$fit-1.96*pred$se.fit,pred$fit+1.96*pred$se.fit)

###########################################
## Presenting the result in transformation
###########################################
matplot(seq(18.5,27,0.1),predlow,type="l",col=c(1,2,2),
        lty=c(1,2,2),lwd = 2,xlab = "Indoor temperature",
        main=paste("Prediction in transformed domain with changing solar radiation"),ylab = "Window opening probability")
matlines(seq(18.5,27,0.1),predmean,type="l",col=c(1,3,3),
         lty=c(1,2,2),lwd = 2)
matlines(seq(18.5,27,0.1),predhigh,type="l",col=c(1,4,4),
         lty=c(1,2,2),lwd = 2)
grid();rug(dat$TempC)

############################################
## Presenting the result in original domain
############################################
matplot(seq(18.5,27,0.1),bt(predlow)$y.l,type="l",col=c(1,2,2),
        lty=c(1,2,2),lwd = 2,xlab = "Indoor temperature",ylim = c(0,1),
        main=paste("Prediction in real domain with changing solar radiation"),ylab = "Window opening probability")
matlines(seq(18.5,27,0.1),bt(predmean)$y.l,type="l",col=c(1,3,3),
         lty=c(1,2,2),lwd = 2)
matlines(seq(18.5,27,0.1),bt(predhigh)$y.l,type="l",col=c(1,4,4),
         lty=c(1,2,2),lwd = 2)
grid();rug(dat$TempC)

##################### temp ~ changing OutdoorTemp
M1 <- Model_O;M1
dat <- dat[dat$WindowClosed==1,]
summary(dat$TempC) #summary(dat[dat$WindowClosed==1,]$TempC)
summary(dat$OutdoorTemp)
pred <- predict(M1,se=TRUE,
                newdata=data.frame(TempC=seq(18.5,27,0.1), CO2C_log=mean(dat$CO2C_log), SolRad_log=mean(dat$SolRad_log), OutdoorTemp=mean(dat$OutdoorTemp)
                ),interval="prediction")
predmean <- cbind(pred$fit,pred$fit-1.96*pred$se.fit,pred$fit+1.96*pred$se.fit)
pred <- predict(M1,se=TRUE,
                newdata=data.frame(TempC=seq(18.5,27,0.1), CO2C_log=mean(dat$CO2C_log), SolRad_log=mean(dat$SolRad_log), OutdoorTemp=3
                ),interval="prediction")
predlow <- cbind(pred$fit,pred$fit-1.96*pred$se.fit,pred$fit+1.96*pred$se.fit)
pred <- predict(M1,se=TRUE,
                newdata=data.frame(TempC=seq(18.5,27,0.1), CO2C_log=mean(dat$CO2C_log), SolRad_log=mean(dat$SolRad_log), OutdoorTemp=13
                ),interval="prediction")
predhigh <- cbind(pred$fit,pred$fit-1.96*pred$se.fit,pred$fit+1.96*pred$se.fit)

###########################################
## Presenting the result in transformation
###########################################
matplot(seq(18.5,27,0.1),predlow,type="l",col=c(1,2,2),
        lty=c(1,2,2),lwd = 2,xlab = "Indoor temperature",
        main=paste("Prediction in transformed domain with changing outdoor temperature"),ylab = "Window opening probability")
matlines(seq(18.5,27,0.1),predmean,type="l",col=c(1,3,3),
         lty=c(1,2,2),lwd = 2)
matlines(seq(18.5,27,0.1),predhigh,type="l",col=c(1,4,4),
         lty=c(1,2,2),lwd = 2)
grid();rug(dat$TempC)

############################################
## Presenting the result in original domain
############################################
matplot(seq(18.5,27,0.1),bt(predlow)$y.l,type="l",col=c(1,2,2),
        lty=c(1,2,2),lwd = 2,xlab = "Indoor temperature",ylim = c(0,1),
        main=paste("Prediction in real domain with changing outdoor temperature"),ylab = "Window opening probability")
matlines(seq(18.5,27,0.1),bt(predmean)$y.l,type="l",col=c(1,3,3),
         lty=c(1,2,2),lwd = 2)
matlines(seq(18.5,27,0.1),bt(predhigh)$y.l,type="l",col=c(1,4,4),
         lty=c(1,2,2),lwd = 2)
grid();rug(dat$TempC)

