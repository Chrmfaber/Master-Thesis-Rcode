library(mise)
mise()
#############
## Timings ##
#############
options(digits=5)

## load data and setup
setwd("C:/Lokal-data/Christian/CMF-DTU/Kurser/11. Semester/Speciale/rCode_Data")
source('Code_MEM_Setup_datClosed.R')

glmer_O <- glmer(WinOpenAction ~ OutdoorTempadj + TempCadj + CO2C_logadj
                 + SolRad_logadj + RHCadj + Season + Room + sin((4*omega)*Hour)
                 + sin((3*omega)*Hour) + sin((2*omega)*Hour) + sin((1*omega)*Hour)
                 + (1+OutdoorTempadj+TempCadj+CO2C_logadj+SolRad_logadj|Dwelling),
                 nAGQ = 0,data = datClosed,family = binomial)

# estimate the model and store results in m
m <- glmer_O;m


se <- sqrt(diag(vcov(m)))
# table of estimates with 95% CI
(tab <- cbind(Est = fixef(m), LL = fixef(m) - 1.96 * se, UL = fixef(m) + 1.96 * se))

exp(tab)

# print the mod results without correlations among fixed effects
print(m, corr = FALSE)
(summary(m)$coefficients)
xtable(summary(m)$coefficients)
summary(m)$varcor$Dwelling
attr(summary(m)$varcor$Dwelling,"stddev")
attr(summary(m)$varcor$Dwelling,"correlation")

xtable(attr(summary(m)$varcor$Dwelling,"stddev"))
xtable(attr(summary(m)$varcor$Dwelling,"correlation"),digits=3)

lattice::dotplot(ranef(m, which = "Dwelling", condVar = TRUE))

####################
# https://slcladal.github.io/mixedregressions.html?fbclid=IwAR0D1gOCZwZ39hwmiC1IxA44A31G3RNpwzRfA5XzkNoVSm8xV5KJyux-LWg#46_visualizing_effects

plot(m, Dwelling ~ resid(.), abline = 0 ) # generate diagnostic plots

plot(m, resid(., type = "pearson") ~ fitted(.) | Dwelling, 
     adj = -0.3, pch = 20, col = "gray40")

ranef(m)
par(mfrow=c(2,3))
qqnorm(ranef(m)$Dwelling[,1]);qqline(ranef(m)$Dwelling[,1])
qqnorm(ranef(m)$Dwelling[,2]);qqline(ranef(m)$Dwelling[,2])
qqnorm(ranef(m)$Dwelling[,3]);qqline(ranef(m)$Dwelling[,3])
qqnorm(ranef(m)$Dwelling[,4]);qqline(ranef(m)$Dwelling[,4])
qqnorm(ranef(m)$Dwelling[,5]);qqline(ranef(m)$Dwelling[,5])
par(mfrow=c(1,1))


library(mise)
mise()
#############
## Timings ##
#############
options(digits=5)

## load data and setup
setwd("C:/Lokal-data/Christian/CMF-DTU/Kurser/11. Semester/Speciale/rCode_Data")
source('Code_MEM_Setup_datOpen.R')

glmer_C <- glmer(WinCloseAction ~ TempCadj + SolRad_logadj + Weekday
                 + OutdoorTempadj + Wind_logadj + cos((2*omega)*Hour)
                 + cos((1*omega)*Hour) + LuxC_logadj + sin((2*omega)*Hour)
                 + Season + SolTimeradj + sin((1*omega)*Hour)+ Room
                 + (1+TempCadj+SolRad_logadj+OutdoorTempadj|Dwelling),
                 nAGQ = 0,data = datOpen,family = binomial)

# estimate the model and store results in m
m <- glmer_C;m


se <- sqrt(diag(vcov(m)))
# table of estimates with 95% CI
(tab <- cbind(Est = fixef(m), LL = fixef(m) - 1.96 * se, UL = fixef(m) + 1.96 * se))

exp(tab)

# print the mod results without correlations among fixed effects
print(m, corr = FALSE)
(summary(m)$coefficients)
xtable(summary(m)$coefficients,digits=3)
summary(m)$varcor$Dwelling
attr(summary(m)$varcor$Dwelling,"stddev")
attr(summary(m)$varcor$Dwelling,"correlation")

xtable(attr(summary(m)$varcor$Dwelling,"stddev"))
xtable(attr(summary(m)$varcor$Dwelling,"correlation"),digits=3)

lattice::dotplot(ranef(m, which = "Dwelling", condVar = TRUE))

####################
# https://slcladal.github.io/mixedregressions.html?fbclid=IwAR0D1gOCZwZ39hwmiC1IxA44A31G3RNpwzRfA5XzkNoVSm8xV5KJyux-LWg#46_visualizing_effects

plot(m, Dwelling ~ resid(.), abline = 0 ) # generate diagnostic plots

plot(m, resid(., type = "pearson") ~ fitted(.) | Dwelling, 
     adj = -0.3, pch = 20, col = "gray40")

ranef(m)
par(mfrow=c(2,2))
qqnorm(ranef(m)$Dwelling[,1]);qqline(ranef(m)$Dwelling[,1])
qqnorm(ranef(m)$Dwelling[,2]);qqline(ranef(m)$Dwelling[,2])
qqnorm(ranef(m)$Dwelling[,3]);qqline(ranef(m)$Dwelling[,3])
qqnorm(ranef(m)$Dwelling[,4]);qqline(ranef(m)$Dwelling[,4])
par(mfrow=c(1,1))