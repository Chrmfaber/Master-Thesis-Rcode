library(mise)
mise()
#############
## Merging ##
#############
options(digits=5)

## load data and have a look
b3 <- read.csv("Data/b3.csv", sep=";")
b4 <- read.csv("Data/b4.csv", sep=";")
b16 <- read.csv("Data/b16.csv", sep=";")

dat1 <- rbind(b3,b4,b16)
dat1$Group <- 1
names(dat1)[4] <- "DateAndTime"

# write to new file
write.csv(dat1, file = "DataMerge/b3b4b16.csv",row.names=FALSE, na="")

#########

## load data and have a look
b1 <- read.csv("Data/b1.csv", sep=";")
b10 <- read.csv("Data/b10.csv", sep=";")

dat2 <- rbind(b1,b10)
dat2$Group <- 2
names(dat2)[4] <- "DateAndTime"

# write to new file
write.csv(dat2, file = "DataMerge/b1b10.csv",row.names=FALSE, na="")

#########

## load data and have a look
b6 <- read.csv("Data/b6.csv", sep=";")
b8 <- read.csv("Data/b8.csv", sep=";")
b9 <- read.csv("Data/b9.csv", sep=";")
b11 <- read.csv("Data/b11.csv", sep=";")
b12 <- read.csv("Data/b12.csv", sep=";")

dat3 <- rbind(b6,b8,b9,b11,b12)
dat3$Group <- 3
names(dat3)[4] <- "DateAndTime"

# write to new file
write.csv(dat3, file = "DataMerge/b6b8b9b11b12.csv",row.names=FALSE, na="")

#########

## load data and have a look
b5 <- read.csv("Data/b5.csv", sep=";")
b7 <- read.csv("Data/b7.csv", sep=";")
b13 <- read.csv("Data/b13.csv", sep=";")
b14 <- read.csv("Data/b14.csv", sep=";")
b15 <- read.csv("Data/b15.csv", sep=";")

dat4 <- rbind(b5,b7,b13,b14,b15)
dat4$Group <- 4
names(dat4)[4] <- "DateAndTime"

# write to new file
write.csv(dat4, file = "DataMerge/b5b7b13b14b15.csv",row.names=FALSE, na="")

# merge all
datAllOrig <- rbind(dat1,dat2,dat3,dat4)

# write to new file
write.csv(datAllOrig, file = "DataMerge/bAllOrig.csv",row.names=FALSE, na="")
