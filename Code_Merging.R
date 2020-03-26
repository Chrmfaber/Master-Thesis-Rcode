library(mise)
mise()
#############
## Merging ##
#############
options(digits=5)

## load data and have a look
b3Clean <- read.csv("DataCleaning/b3Clean.csv", sep=",")
b4Clean <- read.csv("DataCleaning/b4Clean.csv", sep=",")
b16Clean <- read.csv("DataCleaning/b16Clean.csv", sep=",")

dat1 <- rbind(b3Clean,b4Clean,b16Clean)
dat1$Group <- 1

# write to new file
write.csv(dat1, file = "DataMerge/b3b4b16Clean.csv",row.names=FALSE, na="")

#########

## load data and have a look
b1Clean <- read.csv("DataCleaning/b1Clean.csv", sep=",")
b10Clean <- read.csv("DataCleaning/b10Clean.csv", sep=",")

dat2 <- rbind(b1Clean,b10Clean)
dat2$Group <- 2

# write to new file
write.csv(dat2, file = "DataMerge/b1b10Clean.csv",row.names=FALSE, na="")

#########

## load data and have a look
b6Clean <- read.csv("DataCleaning/b6Clean.csv", sep=",")
b8Clean <- read.csv("DataCleaning/b8Clean.csv", sep=",")
b9Clean <- read.csv("DataCleaning/b9Clean.csv", sep=",")
b11Clean <- read.csv("DataCleaning/b11Clean.csv", sep=",")
b12Clean <- read.csv("DataCleaning/b12Clean.csv", sep=",")

dat3 <- rbind(b6Clean,b8Clean,b9Clean,b11Clean,b12Clean)
dat3$Group <- 3

# write to new file
write.csv(dat3, file = "DataMerge/b6b8b9b11b12Clean.csv",row.names=FALSE, na="")

#########

## load data and have a look
b5Clean <- read.csv("DataCleaning/b5Clean.csv", sep=",")
b7Clean <- read.csv("DataCleaning/b7Clean.csv", sep=",")
b13Clean <- read.csv("DataCleaning/b13Clean.csv", sep=",")
b14Clean <- read.csv("DataCleaning/b14Clean.csv", sep=",")
b15Clean <- read.csv("DataCleaning/b15Clean.csv", sep=",")

dat4 <- rbind(b5Clean,b7Clean,b13Clean,b14Clean,b15Clean)
dat4$Group <- 4

# write to new file
write.csv(dat4, file = "DataMerge/b5b7b13b14b15Clean.csv",row.names=FALSE, na="")

# merge all
datAll <- rbind(dat1,dat2,dat3,dat4)

# write to new file
write.csv(datAll, file = "DataMerge/bAllClean.csv",row.names=FALSE, na="")
