library(mise)
mise()
#############################
## Article 2013 recreation ##
#############################
options(digits=7)

## set wd
setwd("C:/Lokal-data/Christian/CMF-DTU/Kurser/11. Semester/Speciale/rCode_Data")

## packages
# https://journal.r-project.org/archive/2017/RJ-2017-066/RJ-2017-066.pdf
# library(readr)
library(xtable)
# library(grid)
# library(gridExtra)
# library(tictoc)
# library(mvtnorm)
# library(car)
# library(corrplot)
# library(TMB)
# library(glmmTMB)
# library(lmerTest)
# library(lsmeans)
par(mfrow=c(1,1),mar=c(3,3,2,2),mgp=c(2,0.7,0))

## functions
# model diagnostics plots
# source('Functions/modelchecks.R')
# source('Functions/stepChisq.R')
# source('Functions/stepLRT.R')
# source('Functions/stepP.R')

## load data and have a look
dat_orig <- read.csv("DataMerge/bAllClean.csv", sep=",")
dat <- dat_orig
#View(dat)

head(dat)
colnames(dat)
str(dat)
summary(dat)

# remove NA rows
#dat <- dat[!is.na(dat$WindowClosed),]
dat <- subset(dat, !is.na(TempC) & !is.na(RHC) & !is.na(CO2C) & !is.na(LuxC) & 
               !is.na(Hour) & !is.na(WeekDay) & !is.na(OutdoorTemp) & !is.na(Wind) & !is.na(SolRad) & 
               !is.na(SolTimer) & !is.na(Room))

#################
# # check if data dimension is identical with article
# temp1 <- dat[dat$WindowClosed == 0 & dat$Group==1,]
# temp2 <- dat[dat$WindowClosed == 1 & dat$Group==1,]
# 
# temp1 <- dat[dat$WindowClosed == 0 & dat$Group==2,]
# temp2 <- dat[dat$WindowClosed == 1 & dat$Group==2,]
# 
# temp1 <- dat[dat$WindowClosed == 0 & dat$Group==3,]
# temp2 <- dat[dat$WindowClosed == 1 & dat$Group==3,]
# 
# temp1 <- dat[dat$WindowClosed == 0 & dat$Group==4,]
# temp2 <- dat[dat$WindowClosed == 1 & dat$Group==4,]

# make variables to factors... fix data variables
dat$Date <- as.POSIXlt(dat$Date)
dat$Day <- as.factor(dat$Day)
dat$Month <- as.factor(dat$Month)
dat$WindowClosed <- as.factor(dat$WindowClosed)
#dat$Hour <- as.factor(dat$Hour)
dat$WeekDay <- as.factor(dat$WeekDay)
dat$NoPresenceRoom <- as.factor(dat$NoPresenceRoom)
dat$NoPresenceDwelling <- as.factor(dat$NoPresenceDwelling)
#dat$WinCloseAction <- as.factor(dat$WinCloseAction)
#dat$WinOpenAction <- as.factor(dat$WinOpenAction)

dat$Dwelling <- as.factor(dat$Dwelling)
dat$Season <- as.factor(dat$Season)
dat$Weekday <- as.factor(dat$Weekday)
dat$TimeOfDay <- as.factor(dat$TimeOfDay)
dat$Group <- as.factor(dat$Group)

#################
# data in each dwelling
for (i in c(1,3:16)) {
  val <- length(which(dat$Dwelling==i))
  table <- cbind(table,c(i,val))
}
table <- table[,-1]

xtable(unname(
  cbind(t(table)[,-1],
        as.matrix(summary(dat[dat$WindowClosed == 0,]$Dwelling)),
        as.matrix(summary(dat[dat$WindowClosed == 1,]$Dwelling)))
))

dim(dat)[1]
dim(dat[dat$WindowClosed == 0,])[1]
dim(dat[dat$WindowClosed == 1,])[1]

n <- dim(dat)[1]
p <- dim(dat)[2]

############
# Table 6 in article 2013:
summary(dat$TempC[dat$WindowClosed==1 & dat$Group==1])
summary(dat$TempC[dat$WindowClosed==0 & dat$Group==1])
sd(dat$TempC[dat$WindowClosed==1 & dat$Group==1])
sd(dat$TempC[dat$WindowClosed==0 & dat$Group==1])

# shortcut
by(dat[, c(20,21,23,15,17,22,16,18,19)], dat[,c("WindowClosed","Group")], summary)

############
# histogram of transforming variables
par(mfrow=c(4,4),mar=c(3,3,2,2),mgp=c(2,0.7,0))
cairo_ps("figs/histvariables.eps", height = 3.9, width = 7)
for (i in c(15,16,17,18,19,20,21,22,23,28,29,30,31)) {
  hist(dat[, i],main = "",xlab = colnames(dat)[i])
}
dev.off()
par(mfrow=c(1,1))

# boxplot of data variables
# The categorical variables c(5,6,7,9,10,25,26,27,32,33)
# colnames(dat[,c(5,6,7,9,10,25,26,27,32,33)])
# "Day"       "Month"     "Room"      "Hour"      "WeekDay"   "Season"    "Weekday"   "TimeOfDay" "Dwelling"  "Group"  
par(mfrow=c(4,4),mar=c(3,3,2,2),mgp=c(2,0.7,0))
cairo_ps("figs/boxplotday.eps", height = 3.9, width = 7)
for (i in c(15,16,17,18,19,20,21,22,23,28,29,30,31)) {
  boxplot(dat[, i]~dat[,5],main = "",xlab = colnames(dat)[5],ylab = colnames(dat)[i],col=1:100)
}
dev.off()
par(mfrow=c(4,4),mar=c(3,3,2,2),mgp=c(2,0.7,0))
cairo_ps("figs/boxplotmonth.eps", height = 3.9, width = 7)
for (i in c(15,16,17,18,19,20,21,22,23,28,29,30,31)) {
  boxplot(dat[, i]~dat[,6],main = "",xlab = colnames(dat)[6],ylab = colnames(dat)[i],col=1:100)
}
dev.off()
par(mfrow=c(4,4),mar=c(3,3,2,2),mgp=c(2,0.7,0))
cairo_ps("figs/boxplotroom.eps", height = 3.9, width = 7)
for (i in c(15,16,17,18,19,20,21,22,23,28,29,30,31)) {
  boxplot(dat[, i]~dat[,7],main = "",xlab = colnames(dat)[7],ylab = colnames(dat)[i],col=1:100)
}
dev.off()
par(mfrow=c(4,4),mar=c(3,3,2,2),mgp=c(2,0.7,0))
cairo_ps("figs/boxplothour.eps", height = 3.9, width = 7)
for (i in c(15,16,17,18,19,20,21,22,23,28,29,30,31)) {
  boxplot(dat[, i]~dat[,9],main = "",xlab = colnames(dat)[9],ylab = colnames(dat)[i],col=1:100)
}
dev.off()
par(mfrow=c(4,4),mar=c(3,3,2,2),mgp=c(2,0.7,0))
cairo_ps("figs/boxplotweekday1.eps", height = 3.9, width = 7)
for (i in c(15,16,17,18,19,20,21,22,23,28,29,30,31)) {
  boxplot(dat[, i]~dat[,10],main = "",xlab = colnames(dat)[10],ylab = colnames(dat)[i],col=1:100)
}
dev.off()
par(mfrow=c(4,4),mar=c(3,3,2,2),mgp=c(2,0.7,0))
cairo_ps("figs/boxplotseason.eps", height = 3.9, width = 7)
for (i in c(15,16,17,18,19,20,21,22,23,28,29,30,31)) {
  boxplot(dat[, i]~dat[,25],main = "",xlab = colnames(dat)[25],ylab = colnames(dat)[i],col=1:100)
}
dev.off()
par(mfrow=c(4,4),mar=c(3,3,2,2),mgp=c(2,0.7,0))
cairo_ps("figs/boxplotweekday2.eps", height = 3.9, width = 7)
for (i in c(15,16,17,18,19,20,21,22,23,28,29,30,31)) {
  boxplot(dat[, i]~dat[,26],main = "",xlab = colnames(dat)[26],ylab = colnames(dat)[i],col=1:100)
}
dev.off()
par(mfrow=c(4,4),mar=c(3,3,2,2),mgp=c(2,0.7,0))
cairo_ps("figs/boxplottimeofday.eps", height = 3.9, width = 7)
for (i in c(15,16,17,18,19,20,21,22,23,28,29,30,31)) {
  boxplot(dat[, i]~dat[,27],main = "",xlab = colnames(dat)[27],ylab = colnames(dat)[i],col=1:100)
}
dev.off()
par(mfrow=c(4,4),mar=c(3,3,2,2),mgp=c(2,0.7,0))
cairo_ps("figs/boxplotdwelling.eps", height = 3.9, width = 7)
for (i in c(15,16,17,18,19,20,21,22,23,28,29,30,31)) {
  boxplot(dat[, i]~dat[,32],main = "",xlab = colnames(dat)[32],ylab = colnames(dat)[i],col=1:100)
}
dev.off()
par(mfrow=c(4,4),mar=c(3,3,2,2),mgp=c(2,0.7,0))
cairo_ps("figs/boxplotgroup.eps", height = 3.9, width = 7)
for (i in c(15,16,17,18,19,20,21,22,23,28,29,30,31)) {
  boxplot(dat[, i]~dat[,33],main = "",xlab = colnames(dat)[33],ylab = colnames(dat)[i],col=1:100)
}
dev.off()
par(mfrow=c(1,1))

############
## log logistic regression for different groups of data
# Variables: Season+Room+TimeOfDay+Weekday+OutdoorTemp+Wind+OutdoorRH+SolRad_log+SolTimer+TempC+RHC+LuxC_log+CO2C_log+DewPtC+Dwelling

# Group 1
m1open <- step(glm(WinOpenAction~1,
                 data=dat[dat$WindowClosed == 1 & dat$Group==1,],family=binomial),
             WinOpenAction ~ TempC + CO2C_log + RHC + LuxC_log + OutdoorTemp + Wind_log + OutdoorRH
             + SolRad_log + SolTimer + Room + TimeOfDay + WeekDay + Season 
             + TempC:TimeOfDay +  CO2C_log:TimeOfDay + RHC:TimeOfDay + LuxC_log:TimeOfDay
             + OutdoorTemp:TimeOfDay + Wind_log:TimeOfDay+ SolRad_log:TimeOfDay
             + TempC:Room + CO2C_log:Room + RHC:Room + LuxC_log:Room 
             + OutdoorTemp:Room + Wind_log:Room + SolRad_log:Room 
             + TempC:Season + CO2C_log:Season + RHC:Season + LuxC_log:Season 
             + OutdoorTemp:Season + Wind_log:Season + SolRad_log:Season, direction="both",
             k = log(length(dat[dat$WindowClosed == 1 & dat$Group==1,]$TempC)))
summary(m1open)
vif(m1open)

m1close <- step(glm(WinCloseAction~1,
                    data=dat[dat$WindowClosed == 0 & dat$Group==1,],family=binomial),
                WinOpenAction ~ TempC + CO2C_log + RHC + LuxC_log + OutdoorTemp + Wind_log + OutdoorRH
                + SolRad_log + SolTimer + Room + TimeOfDay + WeekDay + Season 
                + TempC:TimeOfDay +  CO2C_log:TimeOfDay + RHC:TimeOfDay + LuxC_log:TimeOfDay
                + OutdoorTemp:TimeOfDay + Wind_log:TimeOfDay+ SolRad_log:TimeOfDay
                + TempC:Room + CO2C_log:Room + RHC:Room + LuxC_log:Room 
                + OutdoorTemp:Room + Wind_log:Room + SolRad_log:Room 
                + TempC:Season + CO2C_log:Season + RHC:Season + LuxC_log:Season 
                + OutdoorTemp:Season + Wind_log:Season + SolRad_log:Season, direction="both",
                k = log(length(dat[dat$WindowClosed == 0 & dat$Group==1,]$TempC)))
summary(m1close)
vif(m1close)

############
# Group 2
m2open <- step(glm(WinOpenAction~1,
                   data=dat[dat$WindowClosed == 1 & dat$Group==2,],family=binomial),
               WinOpenAction ~ CO2C_log + RHC +  LuxC_log + Wind_log
               + OutdoorRH + SolTimer + Room + TimeOfDay + WeekDay + Season
               + CO2C_log:TimeOfDay + RHC:TimeOfDay + Wind_log:TimeOfDay
               + CO2C_log:Room + RHC:Room + Wind_log:Room 
               + CO2C_log:Season + RHC:Season + Wind_log:Season,
               direction="both", 
               k=log(length(dat[dat$WindowClosed == 1 & dat$Group==2,]$TempC)))
summary(m2open)
vif(m2open)

m2close <- step(glm(WinCloseAction~1,
                    data=dat[dat$WindowClosed == 0 & dat$Group==2,],family=binomial),
                WinCloseAction ~ CO2C_log + RHC +  LuxC_log + OutdoorTemp + Wind_log
                + OutdoorRH + SolRad_log + SolTimer + Room + TimeOfDay + WeekDay + Season
                + CO2C_log:TimeOfDay + RHC:TimeOfDay + OutdoorTemp:TimeOfDay 
                + Wind_log:TimeOfDay + SolRad_log:TimeOfDay 
                + CO2C_log:Room + RHC:Room + OutdoorTemp:Room
                + Wind_log:Room + SolRad_log:Room 
                + CO2C_log:Season + RHC:Season + OutdoorTemp:Season
                + Wind_log:Season + SolRad_log:Season,
                direction="both", 
                k=log(length(dat[dat$WindowClosed == 0 & dat$Group==2,]$TempC)))
summary(m2close)
vif(m2close)

############
# Group 3
m3open <- step(glm(WinOpenAction~1,
                   data=dat[dat$WindowClosed == 1 & dat$Group==3,],family=binomial),
               WinOpenAction ~ TempC + CO2C_log + RHC + LuxC_log + OutdoorTemp + Wind_log + OutdoorRH
               + SolRad_log + SolTimer + Room + TimeOfDay + WeekDay + Season 
               + TempC:TimeOfDay +  CO2C_log:TimeOfDay + RHC:TimeOfDay + LuxC_log:TimeOfDay
               + OutdoorTemp:TimeOfDay + Wind_log:TimeOfDay+ SolRad_log:TimeOfDay
               + TempC:Room + CO2C_log:Room + RHC:Room + LuxC_log:Room 
               + OutdoorTemp:Room + Wind_log:Room + SolRad_log:Room 
               + TempC:Season + CO2C_log:Season + RHC:Season + LuxC_log:Season 
               + OutdoorTemp:Season + Wind_log:Season + SolRad_log:Season, direction="both",
               k = log(length(dat[dat$WindowClosed == 1 & dat$Group==3,]$TempC)))
summary(m3open)
vif(m3open)

m3close <- step(glm(WinCloseAction~1,
                    data=dat[dat$WindowClosed == 0 & dat$Group==3,],family=binomial),
                WinOpenAction ~ TempC + RHC + LuxC_log + OutdoorTemp + Wind_log + OutdoorRH
               + SolTimer + Room + TimeOfDay + WeekDay + Season 
                + TempC:TimeOfDay + RHC:TimeOfDay + LuxC_log:TimeOfDay
                + OutdoorTemp:TimeOfDay + Wind_log:TimeOfDay
                + TempC:Room + RHC:Room + LuxC_log:Room 
                + OutdoorTemp:Room + Wind_log:Room 
                + TempC:Season + RHC:Season + LuxC_log:Season 
                + OutdoorTemp:Season + Wind_log:Season, direction="both",
                k = log(length(dat[dat$WindowClosed == 0 & dat$Group==3,]$TempC)))
summary(m3close)
vif(m3close)

############
# Group 4
m4open <- step(glm(WinOpenAction~1,
                   data=dat[dat$WindowClosed == 1 & dat$Group==4,],family=binomial),
               WinOpenAction ~ TempC + CO2C_log + RHC +  LuxC_log + OutdoorTemp + Wind_log
               + OutdoorRH + SolRad_log + SolTimer + Room + WeekDay + Season
               + TempC:Room + CO2C_log:Room + RHC:Room + OutdoorTemp:Room 
               + Wind_log:Room + SolRad_log:Room 
               + TempC:Season + CO2C_log:Season + RHC:Season + OutdoorTemp:Season
               + Wind_log:Season + SolRad_log:Season, direction="both",
               k = log(length(dat[dat$WindowClosed == 1 & dat$Group==4,]$TempC)))
summary(m4open)
vif(m4open)


m4close <- step(glm(WinCloseAction~1,
                    data=dat[dat$WindowClosed == 0 & dat$Group==4,],family=binomial),
                WinCloseAction ~ RHC +  OutdoorTemp + Wind_log + OutdoorRH + SolRad_log
                + SolTimer + Room + WeekDay + Season
                + RHC:Room + OutdoorTemp:Room + Wind_log:Room + SolRad_log:Room
                + RHC:Season + OutdoorTemp:Season + Wind_log:Season + SolRad_log:Season, direction="both",
                k = log(length(dat[dat$WindowClosed == 0 & dat$Group==4,]$TempC)))
summary(m4close)
vif(m4close)

############
m1open$call
xtable(summary(m1open))
confint(m1open)
xtable(drop1(m1open,test = "F"))
m1close$call
xtable(summary(m1close))
confint(m1close)
xtable(drop1(m1close,test = "F"))

m2open$call
xtable(summary(m2open))
confint(m2open)
xtable(drop1(m2open,test = "F"))
m2close$call
xtable(summary(m2close))
confint(m2close)
xtable(drop1(m2close,test = "F"))

m3open$call
xtable(summary(m3open))
confint(m3open)
xtable(drop1(m3open,test = "F"))
m3close$call
xtable(summary(m3close))
confint(m3close)
xtable(drop1(m3close,test = "F"))

m4open$call
xtable(summary(m4open))
confint(m4open)
xtable(drop1(m4open,test = "F"))
m4close$call
xtable(summary(m4close))
confint(m4close)
xtable(drop1(m4close,test = "F"))

