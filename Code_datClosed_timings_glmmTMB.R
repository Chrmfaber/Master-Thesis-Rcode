# inserted #bedst glmmTMB model in forward selection for datClosed data set and save timings
timing <- system.time({
  glmmTMBm1 <- glmmTMB(WinOpenAction ~ 1 + (1|Dwelling),
                   data = datClosed,family = binomial)
});# glmmTMBm1$time <- timing
m0 <- c(timing,length(coef(glmmTMBm1)$cond$Dwelling), length(coef(glmmTMBm1)$cond$Dwelling),AIC(glmmTMBm1), BIC(glmmTMBm1), logLik(glmmTMBm1))
timing <- system.time({
  glmmTMBm1r8 <- glmmTMB(WinOpenAction ~ 1 + (1+SolRad_logadj|Dwelling),
                     data = datClosed,family = binomial)
});# glmmTMBm1r8$time <- timing
m1 <- c(timing,length(coef(glmmTMBm1r8)$cond$Dwelling), length(coef(glmmTMBm1r8)$cond$Dwelling),AIC(glmmTMBm1r8), BIC(glmmTMBm1r8), logLik(glmmTMBm1r8))
timing <- system.time({
  glmmTMBm2r2 <- glmmTMB(WinOpenAction ~ 1 + (1+CO2C_logadj+SolRad_logadj|Dwelling),
                     data = datClosed,family = binomial)
});# glmmTMBm2r2$time <- timing
m2<- c(timing,length(coef(glmmTMBm2r2)$cond$Dwelling), length(coef(glmmTMBm2r2)$cond$Dwelling),AIC(glmmTMBm2r2), BIC(glmmTMBm2r2), logLik(glmmTMBm2r2))
timing <- system.time({
  glmmTMBm3v11 <- glmmTMB(WinOpenAction ~ sin((1*omega)*Hour) + (1+CO2C_logadj+SolRad_logadj|Dwelling),
                      data = datClosed,family = binomial)
});# glmmTMBm3v11$time <- timing
m3 <- c(timing,length(coef(glmmTMBm3v11)$cond$Dwelling), length(coef(glmmTMBm3v11)$cond$Dwelling),AIC(glmmTMBm3v11), BIC(glmmTMBm3v11), logLik(glmmTMBm3v11))
timing <- system.time({
  glmmTMBm4r1 <- glmmTMB(WinOpenAction ~ 1 + sin((1*omega)*Hour) + (1+TempCadj+CO2C_logadj+SolRad_logadj|Dwelling),
                     data = datClosed,family = binomial)
});# glmmTMBm4r1$time <- timing
m4 <- c(timing,length(coef(glmmTMBm4r1)$cond$Dwelling), length(coef(glmmTMBm4r1)$cond$Dwelling),AIC(glmmTMBm4r1), BIC(glmmTMBm4r1), logLik(glmmTMBm4r1))
timing <- system.time({
  glmmTMBm5r5 <- glmmTMB(WinOpenAction ~ 1 + sin((1*omega)*Hour) + (1+OutdoorTempadj+TempCadj+CO2C_logadj+SolRad_logadj|Dwelling),
                     data = datClosed,family = binomial)
});# glmmTMBm5r5$time <- timing
m5 <- c(timing,length(coef(glmmTMBm5r5)$cond$Dwelling), length(coef(glmmTMBm5r5)$cond$Dwelling),AIC(glmmTMBm5r5), BIC(glmmTMBm5r5), logLik(glmmTMBm5r5))
timing <- system.time({
  glmmTMBm6v11 <- glmmTMB(WinOpenAction ~ sin((2*omega)*Hour) + sin((1*omega)*Hour) + (1+OutdoorTempadj+TempCadj+CO2C_logadj+SolRad_logadj|Dwelling),
                      data = datClosed,family = binomial)
});# glmmTMBm6v11$time <- timing
m6 <- c(timing,length(coef(glmmTMBm6v11)$cond$Dwelling), length(coef(glmmTMBm6v11)$cond$Dwelling),AIC(glmmTMBm6v11), BIC(glmmTMBm6v11), logLik(glmmTMBm6v11))
timing <- system.time({
  glmmTMBm7v11 <- glmmTMB(WinOpenAction ~ sin((3*omega)*Hour) + sin((2*omega)*Hour) + sin((1*omega)*Hour) + (1+OutdoorTempadj+TempCadj+CO2C_logadj+SolRad_logadj|Dwelling),
                      data = datClosed,family = binomial)
});# glmmTMBm7v11$time <- timing
m7 <- c(timing,length(coef(glmmTMBm7v11)$cond$Dwelling), length(coef(glmmTMBm7v11)$cond$Dwelling),AIC(glmmTMBm7v11), BIC(glmmTMBm7v11), logLik(glmmTMBm7v11))
timing <- system.time({
  glmmTMBm8v11 <- glmmTMB(WinOpenAction ~ sin((4*omega)*Hour) + sin((3*omega)*Hour) + sin((2*omega)*Hour) + sin((1*omega)*Hour) + (1+OutdoorTempadj+TempCadj+CO2C_logadj+SolRad_logadj|Dwelling),
                      data = datClosed,family = binomial)
});# glmmTMBm8v11$time <- timing
m8 <- c(timing,length(coef(glmmTMBm8v11)$cond$Dwelling), length(coef(glmmTMBm8v11)$cond$Dwelling),AIC(glmmTMBm8v11), BIC(glmmTMBm8v11), logLik(glmmTMBm8v11))
timing <- system.time({
  glmmTMBm9v12 <- glmmTMB(WinOpenAction ~ Room + sin((4*omega)*Hour) + sin((3*omega)*Hour) + sin((2*omega)*Hour) + sin((1*omega)*Hour) + (1+OutdoorTempadj+TempCadj+CO2C_logadj+SolRad_logadj|Dwelling),
                      data = datClosed,family = binomial)
});# glmmTMBm9v12$time <- timing
m9 <- c(timing,length(coef(glmmTMBm9v12)$cond$Dwelling), length(coef(glmmTMBm9v12)$cond$Dwelling),AIC(glmmTMBm9v12), BIC(glmmTMBm9v12), logLik(glmmTMBm9v12))
timing <- system.time({
  glmmTMBm10v14 <- glmmTMB(WinOpenAction ~ Season + Room + sin((4*omega)*Hour) + sin((3*omega)*Hour) + sin((2*omega)*Hour) + sin((1*omega)*Hour) + (1+OutdoorTempadj+TempCadj+CO2C_logadj+SolRad_logadj|Dwelling),
                       data = datClosed,family = binomial)
});# glmmTMBm10v14$time <- timing
m10 <- c(timing,length(coef(glmmTMBm10v14)$cond$Dwelling), length(coef(glmmTMBm10v14)$cond$Dwelling),AIC(glmmTMBm10v14), BIC(glmmTMBm10v14), logLik(glmmTMBm10v14))
timing <- system.time({
  glmmTMBm11v3 <- glmmTMB(WinOpenAction ~ RHC + Season + Room + sin((4*omega)*Hour) + sin((3*omega)*Hour) + sin((2*omega)*Hour) + sin((1*omega)*Hour) + (1+OutdoorTempadj+TempCadj+CO2C_logadj+SolRad_logadj|Dwelling),
                      data = datClosed,family = binomial)
});# glmmTMBm11v3$time <- timing
m11 <- c(timing,length(coef(glmmTMBm11v3)$cond$Dwelling), length(coef(glmmTMBm11v3)$cond$Dwelling),AIC(glmmTMBm11v3), BIC(glmmTMBm11v3), logLik(glmmTMBm11v3))
timing <- system.time({
  glmmTMBm11v3_upd <- glmmTMB(WinOpenAction ~ OutdoorTemp + TempC + CO2C_log + SolRad_log
                          + RHC + Season + Room + sin((4*omega)*Hour) + sin((3*omega)*Hour) + sin((2*omega)*Hour) + sin((1*omega)*Hour) + (1+OutdoorTempadj+TempCadj+CO2C_logadj+SolRad_logadj|Dwelling),
                          data = datClosed,family = binomial)
});# glmmTMBm11v3_upd$time <- timing
m12 <- c(timing,length(coef(glmmTMBm11v3_upd)$cond$Dwelling), length(coef(glmmTMBm11v3_upd)$cond$Dwelling),AIC(glmmTMBm11v3_upd), BIC(glmmTMBm11v3_upd), logLik(glmmTMBm11v3_upd))
