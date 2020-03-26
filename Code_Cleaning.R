library(mise)
mise()
##############
## Cleaning ##
##############
options(digits=5)

for (i in c(1,3:16)) {

# title for cleaned data
datasetNum <- i
tmp <- paste0("DataCleaning/b",datasetNum)
(temp <- paste0(tmp,'Clean.csv'))

dir <- paste0("Data/b",datasetNum)
(dirr <- paste0(dir,'.csv'))

## load data and have a look
dat_orig <- read.csv(dirr, sep=";")
dat <- dat_orig
#View(dat)

# head(dat)
# colnames(dat)
# str(dat)
# summary(dat)

names(dat)[4] <- "DateAndTime"

# remove NA rows
dat <- dat[!(is.na(dat$WindowClosed)),]

n <- dim(dat)[1]
p <- dim(dat)[2]

# make variables to factors... fix data variables
dat$Date <- as.Date(dat$Date, origin = "1899-12-30")
dat$Date <- as.POSIXlt(dat$Date)
dat$DateAndTime <- as.Date(dat$DateAndTime, origin = "1899-12-30")

# # Sort data by date
# dat <- dat[order(dat$Date),]
# # summer time fix
# # (idx <- which(dat$Date == "2008-03-30" & dat$Hour == 2)[1])
# # dat$Hour[dat$Hour[idx:n] <= 23] <- dat$Hour[dat$Hour[idx:n] <= 23] + rep(1,length(dat$Hour[dat$Hour[idx:n] <= 23]))
# # dat$Hour[dat$Hour[idx:n] == 24] <- rep(0,length(dat$Hour[dat$Hour[idx:n] == 24]))
# 
# dat$Hour[dat$Hour[(dat$Date == "2008-03-30" & dat$Hour == 2)[1]:n] <= 23] <-
#   dat$Hour[dat$Hour[(dat$Date == "2008-03-30" & dat$Hour == 2)[1]:n] <= 23] 
# + rep(1,length(dat$Hour[dat$Hour[(dat$Date == "2008-03-30" & dat$Hour == 2)[1]:n] <= 23]))
# 
# dat$Hour[dat$Hour[(dat$Date == "2008-03-30" & dat$Hour == 2)[1]:n] == 24] <- rep(0,length(dat$Hour[dat$Hour[(dat$Date == "2008-03-30" & dat$Hour == 2)[1]:n] == 24]))

# Season
for (i in 1:n) {
  if (dat$Month[i] >= 3 && dat$Month[i] <= 5){
    dat$Season[i] <- "Spring"
  }
  if (dat$Month[i] >= 6 && dat$Month[i] <= 8){
    dat$Season[i] <- "Summer"
  }
  if (dat$Month[i] >= 1 && dat$Month[i] <= 2 || dat$Month[i] == 12){
    dat$Season[i] <- "Winter"
  }
}

# Week day 
for (i in 1:n) {
  if (dat$WeekDay[i] >= 1 && dat$WeekDay[i] <= 5){
    dat$Weekday[i] <- "Workday"
  }
  if (dat$WeekDay[i] >= 6 && dat$WeekDay[i] <= 7){
    dat$Weekday[i] <- "Weekend"
  }
}

# Time of day
for (i in 1:n) {
  if (dat$Hour[i] == 23 || dat$Hour[i] < 6){
    dat$TimeOfDay[i] <- "Night"
  }
  if (dat$Hour[i] >= 6 && dat$Hour[i] < 9){
    dat$TimeOfDay[i] <- "Morning"
  }
  if (dat$Hour[i] >= 9 && dat$Hour[i] < 13){
    dat$TimeOfDay[i] <- "Day"
  }
  if (dat$Hour[i] >= 13 && dat$Hour[i] < 18){
    dat$TimeOfDay[i] <- "Afternoon"
  }
  if (dat$Hour[i] >= 18 && dat$Hour[i] < 23){
    dat$TimeOfDay[i] <- "Evening"
  }
}

# transformation
dat$CO2C_log <- log(dat$CO2C)
dat$LuxC_log <- log(dat$LuxC)
dat$Wind_log <- log(dat$Wind+1)
dat$SolRad_log <- log(dat$SolRad+1)

# add variable number for data in a coloumn
dat$Dwelling <- datasetNum

# write to new file
write.csv(dat, file = temp,row.names=FALSE, na="")

}