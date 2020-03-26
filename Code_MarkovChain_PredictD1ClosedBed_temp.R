#################################
# Back transformation functions
# (Normalized and Scaled)
#################################
bt <- function(yt){
  y.bt <- (exp(yt)/(exp(yt)+1))
  return(list(y.l=y.bt))
}

##################### RHC ~ changing SolRad_log
M1 <- Model_C;M1
dat <- dat[dat$WindowClosed==0,]
summary(dat$RHC) #summary(dat[dat$WindowClosed==0,]$RHC)
summary(dat$SolRad_log)
par(mfrow=c(1,1))
pred <- predict(M1,se=TRUE,
                newdata=data.frame(SolRad_log=mean(dat$SolRad_log), SolTimer=mean(dat$SolTimer), OutdoorTemp=mean(dat$OutdoorTemp), RHC=seq(24.5,47,0.1)
                ),interval="prediction")
predmean <- cbind(pred$fit,pred$fit-1.96*pred$se.fit,pred$fit+1.96*pred$se.fit)
pred <- predict(M1,se=TRUE,
                newdata=data.frame(SolRad_log=3, SolTimer=mean(dat$SolTimer), OutdoorTemp=mean(dat$OutdoorTemp), RHC=seq(24.5,47,0.1)
                ),interval="prediction")
predlow <- cbind(pred$fit,pred$fit-1.96*pred$se.fit,pred$fit+1.96*pred$se.fit)
pred <- predict(M1,se=TRUE,
                newdata=data.frame(SolRad_log=6.5, SolTimer=mean(dat$SolTimer), OutdoorTemp=mean(dat$OutdoorTemp), RHC=seq(24.5,47,0.1)
                ),interval="prediction")
predhigh <- cbind(pred$fit,pred$fit-1.96*pred$se.fit,pred$fit+1.96*pred$se.fit)

###########################################
## Presenting the result in transformation
###########################################
matplot(seq(24.5,47,0.1),predlow,type="l",col=c(1,2,2),
        lty=c(1,2,2),main=paste("Variables=mean"),ylim=c(-4.5,-1.5))
matlines(seq(24.5,47,0.1),predmean,type="l",col=c(1,3,3),
         lty=c(1,2,2))
matlines(seq(24.5,47,0.1),predhigh,type="l",col=c(1,4,4),
         lty=c(1,2,2))
grid();rug(dat$RHC)

############################################
## Presenting the result in original domain
############################################
matplot(seq(24.5,47,0.1),bt(predlow)$y.l,type="l",col=c(1,2,2),
        lty=c(1,2,2),main=paste("Variables=mean"),ylim=c(0,0.17))
matlines(seq(24.5,47,0.1),bt(predmean)$y.l,type="l",col=c(1,3,3),
         lty=c(1,2,2))
matlines(seq(24.5,47,0.1),bt(predhigh)$y.l,type="l",col=c(1,4,4),
         lty=c(1,2,2))
grid();rug(dat$RHC)

##################### RHC ~ changing SolTimer
M1 <- Model_C;M1
dat <- dat[dat$WindowClosed==0,]
summary(dat$RHC) #summary(dat[dat$WindowClosed==0,]$RHC)
summary(dat$SolTimer)
par(mfrow=c(1,1))
pred <- predict(M1,se=TRUE,
                newdata=data.frame(SolRad_log=mean(dat$SolRad_log), SolTimer=mean(dat$SolTimer), OutdoorTemp=mean(dat$OutdoorTemp), RHC=seq(24.5,47,0.1)
                ),interval="prediction")
predmean <- cbind(pred$fit,pred$fit-1.96*pred$se.fit,pred$fit+1.96*pred$se.fit)
pred <- predict(M1,se=TRUE,
                newdata=data.frame(SolRad_log=mean(dat$SolRad_log), SolTimer=5, OutdoorTemp=mean(dat$OutdoorTemp), RHC=seq(24.5,47,0.1)
                ),interval="prediction")
predlow <- cbind(pred$fit,pred$fit-1.96*pred$se.fit,pred$fit+1.96*pred$se.fit)
pred <- predict(M1,se=TRUE,
                newdata=data.frame(SolRad_log=mean(dat$SolRad_log), SolTimer=14, OutdoorTemp=mean(dat$OutdoorTemp), RHC=seq(24.5,47,0.1)
                ),interval="prediction")
predhigh <- cbind(pred$fit,pred$fit-1.96*pred$se.fit,pred$fit+1.96*pred$se.fit)

###########################################
## Presenting the result in transformation
###########################################
matplot(seq(24.5,47,0.1),predlow,type="l",col=c(1,2,2),
        lty=c(1,2,2),main=paste("Variables=mean"))
matlines(seq(24.5,47,0.1),predmean,type="l",col=c(1,3,3),
         lty=c(1,2,2))
matlines(seq(24.5,47,0.1),predhigh,type="l",col=c(1,4,4),
         lty=c(1,2,2))
grid();rug(dat$RHC)

############################################
## Presenting the result in original domain
############################################
matplot(seq(24.5,47,0.1),bt(predlow)$y.l,type="l",col=c(1,2,2),
        lty=c(1,2,2),main=paste("Variables=mean"))
matlines(seq(24.5,47,0.1),bt(predmean)$y.l,type="l",col=c(1,3,3),
         lty=c(1,2,2))
matlines(seq(24.5,47,0.1),bt(predhigh)$y.l,type="l",col=c(1,4,4),
         lty=c(1,2,2))
grid();rug(dat$RHC)

##################### RHC ~ changing OutdoorTemp
M1 <- Model_C;M1
dat <- dat[dat$WindowClosed==0,]
summary(dat$RHC) #summary(dat[dat$WindowClosed==0,]$RHC)
summary(dat$OutdoorTemp)
par(mfrow=c(1,1))
pred <- predict(M1,se=TRUE,
                newdata=data.frame(SolRad_log=mean(dat$SolRad_log), SolTimer=mean(dat$SolTimer), OutdoorTemp=mean(dat$OutdoorTemp), RHC=seq(24.5,47,0.1)
                ),interval="prediction")
predmean <- cbind(pred$fit,pred$fit-1.96*pred$se.fit,pred$fit+1.96*pred$se.fit)
pred <- predict(M1,se=TRUE,
                newdata=data.frame(SolRad_log=mean(dat$SolRad_log), SolTimer=mean(dat$SolTimer), OutdoorTemp=5, RHC=seq(24.5,47,0.1)
                ),interval="prediction")
predlow <- cbind(pred$fit,pred$fit-1.96*pred$se.fit,pred$fit+1.96*pred$se.fit)
pred <- predict(M1,se=TRUE,
                newdata=data.frame(SolRad_log=mean(dat$SolRad_log), SolTimer=mean(dat$SolTimer), OutdoorTemp=20, RHC=seq(24.5,47,0.1)
                ),interval="prediction")
predhigh <- cbind(pred$fit,pred$fit-1.96*pred$se.fit,pred$fit+1.96*pred$se.fit)

###########################################
## Presenting the result in transformation
###########################################
matplot(seq(24.5,47,0.1),predlow,type="l",col=c(1,2,2),
        lty=c(1,2,2),main=paste("Variables=mean"),ylim = c(-5.5,0.5))
matlines(seq(24.5,47,0.1),predmean,type="l",col=c(1,3,3),
         lty=c(1,2,2))
matlines(seq(24.5,47,0.1),predhigh,type="l",col=c(1,4,4),
         lty=c(1,2,2))
grid();rug(dat$RHC)

############################################
## Presenting the result in original domain
############################################
matplot(seq(24.5,47,0.1),bt(predlow)$y.l,type="l",col=c(1,2,2),
        lty=c(1,2,2),main=paste("Variables=mean"),ylim=c(0,0.6))
matlines(seq(24.5,47,0.1),bt(predmean)$y.l,type="l",col=c(1,3,3),
         lty=c(1,2,2))
matlines(seq(24.5,47,0.1),bt(predhigh)$y.l,type="l",col=c(1,4,4),
         lty=c(1,2,2))
grid();rug(dat$RHC)

