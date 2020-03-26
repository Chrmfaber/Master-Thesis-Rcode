library(mise)
mise()

source('Code_MC_Setup.R')
source('Code_MarkovChain_Fun.R')
dat_orig <- dat

# Choosing dwelling nr 1-16 (except the missing nr 2)
dat <- dat_orig[dat_orig$Dwelling==1,]

dat <- dat[dat$Room=="Livingroom",]
ModelD1_O <- glm(formula = WinOpenAction ~ TempC + CO2C_log
                 + SolRad_log + OutdoorTemp # + Room
                 , family = binomial, data = dat[dat$WindowClosed==1,])
M1 <- ModelD1_O

# Choosing dwelling nr 1-16 (except the missing nr 2)
dat <- dat_orig[dat_orig$Dwelling==1,]

dat <- dat[dat$Room=="Livingroom",]

#center and normalize
n <- dim(dat)[1]
one <- rep(1,n)
x <- as.matrix(dat[,c(20,28,31,15)])
meanx <- drop(one %*% x)/n
normx <- rep(1, length(meanx) )#sqrt(drop(one %*% (as.matrix(dat[,c(20,28,31,15)])^2)))
datscaled <- scale(x, meanx, normx)# normalize training data subtracting mean and dividing by L2 norm

WinOpenAction <- dat$WinOpenAction
WinCloseAction <- dat$WinCloseAction
WindowClosed <- dat$WindowClosed
datscaled <- cbind(datscaled,WinOpenAction,WinCloseAction,WindowClosed)
datscaled <- as.data.frame(datscaled)

datscaled$WindowClosed[datscaled$WindowClosed == 2] <- 0

ModelD1_O <- glm(formula = WinOpenAction ~ TempC + CO2C_log
                 + SolRad_log + OutdoorTemp # + Room
                 , family = binomial, data = datscaled[datscaled$WindowClosed==0,])
M2 <- ModelD1_O


# Choosing dwelling nr 1-16 (except the missing nr 2)
dat <- dat_orig[dat_orig$Dwelling==1,]

dat <- dat[dat$Room=="Livingroom",]

dat$TempCcen <- dat$TempC-mean(dat$TempC)
dat$CO2C_logcen <- dat$CO2C_log-mean(dat$CO2C_log)
dat$SolRad_logcen <- dat$SolRad_log-mean(dat$SolRad_log)
dat$OutdoorTempcen <- dat$OutdoorTemp-mean(dat$OutdoorTemp)

ModelD1_O <- glm(formula = WinOpenAction ~ TempCcen + CO2C_logcen
                 + SolRad_logcen + OutdoorTempcen # + Room
                 , family = binomial, data = dat[dat$WindowClosed==1,])
M3 <- ModelD1_O

################

cbind(coef(M1),coef(M2),coef(M3))

cov2cor(summary(M1)$cov.unscaled)
kappa(cov2cor(summary(M1)$cov.unscaled))
#cov2cor(summary(M2)$cov.unscaled)
kappa(cov2cor(summary(M2)$cov.unscaled))
#cov2cor(summary(M13)$cov.unscaled)
kappa(cov2cor(summary(M3)$cov.unscaled))

A <- matrix(0.75,ncol=5,nrow=5);diag(A) <- 1;A;kappa(A)
A <- matrix(0.80,ncol=5,nrow=5);diag(A) <- 1;kappa(A)
A <- matrix(0.90,ncol=5,nrow=5);diag(A) <- 1;kappa(A)
A <- matrix(0.95,ncol=5,nrow=5);diag(A) <- 1;kappa(A)
A <- matrix(0.999,ncol=5,nrow=5);diag(A) <- 1;kappa(A)
A <- matrix(0.9998,ncol=5,nrow=5);diag(A) <- 1;kappa(A)




kappa(cov2cor(summary(M1)$cov.unscaled))
kappa(cov2cor(summary(M1)$cov.unscaled[-1,-1]))









# m1 <- lm(CO2C_log~TempC+SolRad_log+OutdoorTemp,data = dat[dat$WindowClosed==1,])
# m1 <- lm(TempC~CO2C_log+SolRad_log+OutdoorTemp,data = dat[dat$WindowClosed==1,])
# m1
# 
# summary(m1)$cov.unscaled
# cov2cor(summary(m1)$cov.unscaled)
# kappa(cov2cor(summary(m1)$cov.unscaled))
