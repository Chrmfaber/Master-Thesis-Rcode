library(mise)
mise()

datClosed_timings_glmmTMB <- read.csv("DataTimings/datClosed_timings_glmmTMB.csv", sep=",")
datClosed_timings_glmer_nAGQ0 <- read.csv("DataTimings/datClosed_timings_glmer_nAGQ0.csv", sep=",")
datClosed_timings_glmer_nAGQ1 <- read.csv("DataTimings/datClosed_timings_glmer_nAGQ1.csv", sep=",")

datClosed_timings_glmmTMB[,6] <- 1:13
datClosed_timings_glmer_nAGQ0[,6] <- 1:13
datClosed_timings_glmer_nAGQ1[,6] <- 1:13

par(mfrow=c(1,1),mar=c(3,3,2,2),mgp=c(2,0.7,0))
par(mfrow=c(2,1))

plot(datClosed_timings_glmer_nAGQ1$FixedEf,datClosed_timings_glmer_nAGQ1$elapsed,
     main = "Evaluation time for MEM evaluation for opening actions",xlab = "Numbers of fixed and random effects",ylab = "evaluation time [sec]",type = "o",col=2);grid()
lines(datClosed_timings_glmmTMB$FixedEf,datClosed_timings_glmmTMB$elapsed,type = "o",col=3)
lines(datClosed_timings_glmer_nAGQ0$FixedEf,datClosed_timings_glmer_nAGQ0$elapsed,type = "o",col=4)
legend("topleft", legend=c("glmer1","glmmTMB","glmer0"), lwd=1,col=c("red","green","blue"),y.intersp = 0.75,x.intersp = 0.1,pch = 1)



plot(datClosed_timings_glmer_nAGQ1$FixedEf[1:5],datClosed_timings_glmer_nAGQ1$elapsed[1:5],
     main = "Evaluation time for MEM evaluation for opening actions",xlab = "Numbers of fixed and random effects",ylab = "evaluation time [sec]",type = "o",col=2);grid()
lines(datClosed_timings_glmmTMB$FixedEf[1:5],datClosed_timings_glmmTMB$elapsed[1:5],type = "o",col=3)
lines(datClosed_timings_glmer_nAGQ0$FixedEf[1:5],datClosed_timings_glmer_nAGQ0$elapsed[1:5],type = "o",col=4)
legend("topleft", legend=c("glmer1","glmmTMB","glmer0"), lwd=1,col=c("red","green","blue"),y.intersp = 0.75,x.intersp = 0.1,pch = 1)

library(xtable)
datClosed_timings_glmmTMB
datClosed_timings_glmmTMB[,8:10]
xtable(datClosed_timings_glmmTMB[,8:10])

#############
#############
#############

library(mise)
mise()

datOpen_timings_glmmTMB <- read.csv("DataTimings/datOpen_timings_glmmTMB.csv", sep=",")
datOpen_timings_glmer_nAGQ0 <- read.csv("DataTimings/datOpen_timings_glmer_nAGQ0.csv", sep=",")
datOpen_timings_glmer_nAGQ1 <- read.csv("DataTimings/datOpen_timings_glmer_nAGQ1.csv", sep=",")

datOpen_timings_glmmTMB[,6] <- 1:16
datOpen_timings_glmer_nAGQ0[,6] <- 1:16
datOpen_timings_glmer_nAGQ1[,6] <- 1:16

par(mfrow=c(1,1),mar=c(3,3,2,2),mgp=c(2,0.7,0))
par(mfrow=c(2,1))

plot(datOpen_timings_glmer_nAGQ1$FixedEf,datOpen_timings_glmer_nAGQ1$elapsed,
     main = "Evaluation time for MEM evaluation for closing actions",xlab = "Numbers of fixed and random effects",ylab = "evaluation time [sec]",type = "o",col=2);grid()
lines(datOpen_timings_glmmTMB$FixedEf,datOpen_timings_glmmTMB$elapsed,type = "o",col=3)
lines(datOpen_timings_glmer_nAGQ0$FixedEf,datOpen_timings_glmer_nAGQ0$elapsed,type = "o",col=4)
legend("topleft", legend=c("glmer1","glmmTMB","glmer0"), lwd=1,col=c("red","green","blue"),y.intersp = 0.75,x.intersp = 0.1,pch = 1)



plot(datOpen_timings_glmer_nAGQ1$FixedEf[1:5],datOpen_timings_glmer_nAGQ1$elapsed[1:5],
     main = "Evaluation time for MEM evaluation for closing actions",xlab = "Numbers of fixed and random effects",ylab = "evaluation time [sec]",type = "o",col=2);grid()
lines(datOpen_timings_glmmTMB$FixedEf[1:5],datOpen_timings_glmmTMB$elapsed[1:5],type = "o",col=3)
lines(datOpen_timings_glmer_nAGQ0$FixedEf[1:5],datOpen_timings_glmer_nAGQ0$elapsed[1:5],type = "o",col=4)
legend("topleft", legend=c("glmer1","glmmTMB","glmer0"), lwd=1,col=c("red","green","blue"),y.intersp = 0.75,x.intersp = 0.1,pch = 1)

library(xtable)
datOpen_timings_glmmTMB
datOpen_timings_glmmTMB[,8:10]
xtable(datOpen_timings_glmmTMB[,8:10])
