library(mise)
mise()
#############
## Timings ##
#############
options(digits=5)

## load data and setup
setwd("C:/Lokal-data/Christian/CMF-DTU/Kurser/11. Semester/Speciale/rCode_Data")
source('Code_MEM_Setup_datClosed.R')

# inserted #bedst glmmTMB model in forward selection for datClosed data set and save timings
source('Code_datClosed_timings_glmmTMB.R')

Timings <- rbind(m0,m1,m2,m3,m4,m5,m6,m7,m8,m9,m10,m11,m12)
colnames(Timings) <- c(colnames(Timings)[1:5],"FixedEf","RandomEf","AIC","BIC","logLik")
Timings

# write to new file
write.csv(Timings, file = "DataTimings/datClosed_timings_glmmTMB.csv",row.names=FALSE, na="")

#################

library(mise)
mise()
#############
## Timings ##
#############
options(digits=5)

## load data and setup
setwd("C:/Lokal-data/Christian/CMF-DTU/Kurser/11. Semester/Speciale/rCode_Data")
source('Code_MEM_Setup_datClosed.R')

# inserted #bedst glmmTMB model in forward selection for datClosed data set and save timings
source('Code_datClosed_timings_glmer_nAGQ1.R')

Timings <- rbind(m0,m1,m2,m3,m4,m5,m6,m7,m8,m9,m10,m11,m12)
colnames(Timings) <- c(colnames(Timings)[1:5],"FixedEf","RandomEf","AIC","BIC","logLik")
Timings

# write to new file
write.csv(Timings, file = "DataTimings/datClosed_timings_glmer_nAGQ1.csv",row.names=FALSE, na="")

#################

library(mise)
mise()
#############
## Timings ##
#############
options(digits=5)

## load data and setup
setwd("C:/Lokal-data/Christian/CMF-DTU/Kurser/11. Semester/Speciale/rCode_Data")
source('Code_MEM_Setup_datClosed.R')

# inserted #bedst glmmTMB model in forward selection for datClosed data set and save timings
source('Code_datClosed_timings_glmer_nAGQ0.R')

Timings <- rbind(m0,m1,m2,m3,m4,m5,m6,m7,m8,m9,m10,m11,m12)
colnames(Timings) <- c(colnames(Timings)[1:5],"FixedEf","RandomEf","AIC","BIC","logLik")
Timings

# write to new file
write.csv(Timings, file = "DataTimings/datClosed_timings_glmer_nAGQ0.csv",row.names=FALSE, na="")

#################

# check writing to file
#alook <- read.csv("DataTimings/glmmTMB_datOpen_timings.csv", sep=",")
#View(alook)

#length(coef(test3)$cond$Dwelling) #count amount of fixed effects
#length(ranef(test3)$cond$Dwelling) #count amount of random effects

#plot(Timings$FixedEf+Timings$RandomEf,Timings$time,type = "o",col=3,lwd=2);grid()
