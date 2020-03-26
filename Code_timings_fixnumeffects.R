library(mise)
mise()
datOpen_timings_glmmTMB <- read.csv("DataTimings/datOpen_timings_glmmTMB.csv", sep=",")
datOpen_timings_glmer_nAGQ0 <- read.csv("DataTimings/datOpen_timings_glmer_nAGQ0.csv", sep=",")
datOpen_timings_glmer_nAGQ1 <- read.csv("DataTimings/datOpen_timings_glmer_nAGQ1.csv", sep=",")

datOpen_timings_glmer_nAGQ0$FixedEf <- datOpen_timings_glmmTMB$FixedEf
datOpen_timings_glmer_nAGQ0$RandomEf <- datOpen_timings_glmmTMB$RandomEf
datOpen_timings_glmer_nAGQ1$FixedEf <- datOpen_timings_glmmTMB$FixedEf
datOpen_timings_glmer_nAGQ1$RandomEf <- datOpen_timings_glmmTMB$RandomEf

# write to new file
write.csv(datOpen_timings_glmer_nAGQ0, file = "DataTimings/datOpen_timings_glmer_nAGQ0.csv",row.names=FALSE, na="")

# write to new file
write.csv(datOpen_timings_glmer_nAGQ1, file = "DataTimings/datOpen_timings_glmer_nAGQ1.csv",row.names=FALSE, na="")

#################
#################
#################

library(mise)
mise()
datClosed_timings_glmmTMB <- read.csv("DataTimings/datClosed_timings_glmmTMB.csv", sep=",")
datClosed_timings_glmer_nAGQ0 <- read.csv("DataTimings/datClosed_timings_glmer_nAGQ0.csv", sep=",")
datClosed_timings_glmer_nAGQ1 <- read.csv("DataTimings/datClosed_timings_glmer_nAGQ1.csv", sep=",")

datClosed_timings_glmer_nAGQ0$FixedEf <- datClosed_timings_glmmTMB$FixedEf
datClosed_timings_glmer_nAGQ0$RandomEf <- datClosed_timings_glmmTMB$RandomEf
datClosed_timings_glmer_nAGQ1$FixedEf <- datClosed_timings_glmmTMB$FixedEf
datClosed_timings_glmer_nAGQ1$RandomEf <- datClosed_timings_glmmTMB$RandomEf

# write to new file
write.csv(datClosed_timings_glmer_nAGQ0, file = "DataTimings/datClosed_timings_glmer_nAGQ0.csv",row.names=FALSE, na="")

# write to new file
write.csv(datClosed_timings_glmer_nAGQ1, file = "DataTimings/datClosed_timings_glmer_nAGQ1.csv",row.names=FALSE, na="")
