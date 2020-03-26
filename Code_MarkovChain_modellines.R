dat <- dat[dat$Room=="Livingroom",]
#dat <- dat[dat$Room=="Bedroom",]
ModelD1_O <- glm(formula = WinOpenAction ~ TempC + CO2C_log
                 + SolRad_log + OutdoorTemp # + Room
                 , family = binomial, data = dat[dat$WindowClosed==1,])
ModelD1_C <- glm(formula = WinCloseAction ~ SolRad_log + SolTimer # + Room
                 + OutdoorTemp + RHC, family = binomial, data = dat[dat$WindowClosed==0,])
Model_O <- ModelD1_O
Model_C <- ModelD1_C

#####################
dat <- dat[dat$Room=="Livingroom",]
# "Bedroom"    "Livingroom"
dat <- dat[dat$TimeOfDay=="Day",]
# "Afternoon" "Day"       "Evening"   "Morning"   "Night"
ModelD3_O <- glm(formula = WinOpenAction ~ 1 # TimeOfDay + Room
                 , family = binomial, data = dat[dat$WindowClosed==1,])
ModelD3_C <- glm(formula = WinCloseAction ~ OutdoorTemp # + Room
                 , family = binomial, data = dat[dat$WindowClosed==0,])
Model_O <- ModelD3_O
Model_C <- ModelD3_C

#####################
dat <- dat[dat$Room=="Livingroom",]
ModelD4_O <- glm(formula = WinOpenAction ~ LuxC_log + OutdoorTemp # + Room
                 + CO2C_log + SolRad_log, family = binomial, data = dat[dat$WindowClosed==1,])
ModelD4_C <- glm(formula = WinCloseAction ~ OutdoorTemp + sin((1 * omega) * Hour) # + Room
                 , family = binomial, data = dat[dat$WindowClosed==0,])
Model_O <- ModelD4_O
Model_C <- ModelD4_C

#####################
ModelD5_O <- glm(formula = WinOpenAction ~ SolRad_log + sin((1 * omega) * Hour)
                 + CO2C_log + Room, family = binomial, data = dat[dat$WindowClosed == 1, ])
ModelD5_C <- glm(formula = WinCloseAction ~ Room + TempC + Season + Wind_log, 
                 family = binomial, data = dat[dat$WindowClosed == 0, ])
Model_O <- ModelD5_O
Model_C <- ModelD5_C

#####################
ModelD6_O <- glm(formula = WinOpenAction ~ TimeOfDay + CO2C_log + TempC + 
                   cos((1 * omega) * Hour) + sin((1 * omega) * Hour), family = binomial, 
                 data = dat[dat$WindowClosed == 1, ])
ModelD6_C <- glm(formula = WinCloseAction ~ CO2C_log + Room + cos((1 * omega) * Hour)
                 + TempC + SolTimer, family = binomial, data = dat[dat$WindowClosed == 0, ])
Model_O <- ModelD6_O
Model_C <- ModelD6_C

#####################
ModelD7_O <- glm(formula = WinOpenAction ~ TimeOfDay + SolRad_log + TempC + 
                   sin((1 * omega) * Hour) + CO2C_log + Room + Season, family = binomial, 
                 data = dat[dat$WindowClosed == 1, ])
ModelD7_C <- glm(formula = WinCloseAction ~ OutdoorTemp + Room + sin((1 * omega) * Hour)
                 + cos((1 * omega) * Hour) + SolTimer + Season + CO2C_log + RHC
                 + OutdoorRH, family = binomial, data = dat[dat$WindowClosed == 0, ])
Model_O <- ModelD7_O
Model_C <- ModelD7_C

#####################
ModelD8_O <- glm(formula = WinOpenAction ~ TimeOfDay + CO2C_log + sin((1 * omega) * Hour)
                 + Room + LuxC_log + OutdoorTemp, family = binomial,
                 data = dat[dat$WindowClosed == 1, ])
ModelD8_C <- glm(formula = WinCloseAction ~ Room + cos((1 * omega) * Hour)
                 + TempC, family = binomial, data = dat[dat$WindowClosed == 0, ])
Model_O <- ModelD8_O
Model_C <- ModelD8_C

#####################
ModelD9_O <- glm(formula = WinOpenAction ~ CO2C_log + SolRad_log, family = binomial, 
                 data = dat[dat$WindowClosed == 1, ])
ModelD9_C <- glm(formula = WinCloseAction ~ TimeOfDay + Room, family = binomial, 
                 data = dat[dat$WindowClosed == 0, ])
Model_O <- ModelD9_O
Model_C <- ModelD9_C

#####################
ModelD10_O <- glm(formula = WinCloseAction ~ Room + SolTimer, family = binomial, 
                  data = dat[dat$WindowClosed == 0, ])
ModelD10_C <- glm(formula = WinCloseAction ~ Room + SolTimer, family = binomial, 
                  data = dat[dat$WindowClosed == 0, ])
Model_O <- ModelD10_O
Model_C <- ModelD10_C

#####################
ModelD11_O <- glm(formula = WinOpenAction ~ TimeOfDay + CO2C_log + TempC + 
                    cos((1 * omega) * Hour), family = binomial,
                  data = dat[dat$WindowClosed == 1, ])
ModelD11_C <- glm(formula = WinCloseAction ~ Room + TempC + SolTimer,
                  family = binomial, data = dat[dat$WindowClosed == 0, ])
Model_O <- ModelD11_O
Model_C <- ModelD11_C

#####################
ModelD12_O <- glm(formula = WinOpenAction ~ 1, family = binomial,
                  data = dat[dat$WindowClosed == 1, ])
ModelD12_C <- glm(formula = WinCloseAction ~ Room + TempC + SolTimer, family = binomial, 
                  data = dat[dat$WindowClosed == 0, ])
Model_O <- ModelD12_O
Model_C <- ModelD12_C

#####################
ModelD13_O <- glm(formula = WinOpenAction ~ LuxC_log + OutdoorTemp,
                  family = binomial, data = dat[dat$WindowClosed == 1, ])
ModelD13_C <- glm(formula = WinCloseAction ~ Room + OutdoorTemp + SolRad_log + 
                    TimeOfDay + TempC + SolTimer + sin((1 * omega) * Hour),
                  family = binomial, data = dat[dat$WindowClosed == 0, ])
Model_O <- ModelD13_O
Model_C <- ModelD13_C

#####################
ModelD14_O <- glm(formula = WinOpenAction ~ TimeOfDay + TempC + sin((1 * omega) * Hour),
                  family = binomial, data = dat[dat$WindowClosed == 1, ])
ModelD14_C <- glm(formula = WinCloseAction ~ TimeOfDay + OutdoorTemp + Room + 
                    RHC + LuxC_log + OutdoorRH, family = binomial,
                  data = dat[dat$WindowClosed == 0, ])
Model_O <- ModelD14_O
Model_C <- ModelD14_C

#####################
ModelD15_O <- glm(formula = WinOpenAction ~ sin((1 * omega) * Hour) + TimeOfDay
                  + CO2C_log + SolRad_log + RHC + SolTimer + cos((1 * omega) * Hour),
                  family = binomial, data = dat[dat$WindowClosed == 1, ])
ModelD15_C <- glm(formula = WinCloseAction ~ TempC + Season + SolTimer
                  + TimeOfDay + RHC + CO2C_log + Wind_log,
                  family = binomial, data = dat[dat$WindowClosed == 0, ])
Model_O <- ModelD15_O
Model_C <- ModelD15_C

#####################
ModelD16_O <- glm(formula = WinOpenAction ~ sin((1 * omega) * Hour) + TimeOfDay + 
                    CO2C_log + SolRad_log + RHC + SolTimer + cos((1 * omega) * Hour),
                  family = binomial, data = dat[dat$WindowClosed == 1, ])
ModelD16_C <- glm(formula = WinCloseAction ~ TempC + Season + SolTimer + TimeOfDay + 
                    RHC + CO2C_log + Wind_log,
                  family = binomial, data = dat[dat$WindowClosed == 0, ])
Model_O <- ModelD16_O
Model_C <- ModelD16_C

#####################
