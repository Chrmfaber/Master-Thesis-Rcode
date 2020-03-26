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
# library(xtable)
# library(grid)
# library(gridExtra)
# library(tictoc)
# library(mvtnorm)
# library(car)
# library(corrplot)
# library(TMB)
# library(glmmTMB)
# library(beepr)
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

#################

n <- dim(dat)[1]
p <- dim(dat)[2]

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

############
# Table 6 in article 2013:
summary(dat$TempC[dat$WindowClosed==1 & dat$Group==1])
summary(dat$TempC[dat$WindowClosed==0 & dat$Group==1])

by(dat[, c(20,21,23,15,17,22,16,18,19)], dat[,c("WindowClosed","Group")], summary)

############
summary(dat[dat$WindowClosed == 0,]$Dwelling) # dwelling 12 miss values
summary(dat[dat$WindowClosed == 1,]$Dwelling)

## Mixed effect models regression with groups and dwellings
datOpen <- subset(dat, Dwelling != 12)
datOpen$Dwelling <- factor(datOpen$Dwelling)
table(datOpen$Dwelling)
datOpen <- datOpen[datOpen$WindowClosed == 0,]

datClosed <- subset(dat, Dwelling != 12)
datClosed$Dwelling <- factor(datClosed$Dwelling)
table(datClosed$Dwelling)
datClosed <- datClosed[datClosed$WindowClosed == 1,]

# Fourier series for T-period intervals
T_int <- 24
omega <- 2*pi/T_int

