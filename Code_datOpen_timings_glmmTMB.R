# inserted #bedst glmmTMB model in forward selection for datOpen data set and save timings
timing <- system.time({
  glmmTMBm1 <- glmmTMB(WinCloseAction ~ 1 + (1|Dwelling),
                       data = datOpen,family = binomial)
});# glmmTMBm1$time <- timing
m0 <- c(timing,length(coef(glmmTMBm1)$cond$Dwelling), length(coef(glmmTMBm1)$cond$Dwelling),AIC(glmmTMBm1), BIC(glmmTMBm1), logLik(glmmTMBm1))
timing <- system.time({
  glmmTMBm1r5 <- glmmTMB(WinCloseAction ~ 1 + (1+OutdoorTempadj|Dwelling),
                         data = datOpen,family = binomial)
});# glmmTMBm1r5$time <- timing
m1 <- c(timing,length(coef(glmmTMBm1r5)$cond$Dwelling), length(coef(glmmTMBm1r5)$cond$Dwelling),AIC(glmmTMBm1r5), BIC(glmmTMBm1r5), logLik(glmmTMBm1r5))
timing <- system.time({
  glmmTMBm2r8 <- glmmTMB(WinCloseAction ~ 1 + (1+SolRad_logadj+OutdoorTempadj|Dwelling),
                         data = datOpen,family = binomial)
});# glmmTMBm2r8$time <- timing
m2 <- c(timing,length(coef(glmmTMBm2r8)$cond$Dwelling), length(coef(glmmTMBm2r8)$cond$Dwelling),AIC(glmmTMBm2r8), BIC(glmmTMBm2r8), logLik(glmmTMBm2r8))
timing <- system.time({
  glmmTMBm3r1 <- glmmTMB(WinCloseAction ~ 1 + (1+TempCadj+SolRad_logadj+SolRad_logadj+OutdoorTempadj|Dwelling),
                         data = datOpen,family = binomial)
});# glmmTMBm3r1$time <- timing
m3 <- c(timing,length(coef(glmmTMBm3r1)$cond$Dwelling), length(coef(glmmTMBm3r1)$cond$Dwelling),AIC(glmmTMBm3r1), BIC(glmmTMBm3r1), logLik(glmmTMBm3r1))
timing <- system.time({
  glmmTMBm4v12 <- glmmTMB(WinCloseAction ~ Room + (1+TempCadj+SolRad_logadj+OutdoorTempadj|Dwelling),
                          data = datOpen,family = binomial)
});# glmmTMBm4v12$time <- timing
m4 <- c(timing,length(coef(glmmTMBm4v12)$cond$Dwelling), length(coef(glmmTMBm4v12)$cond$Dwelling),AIC(glmmTMBm4v12), BIC(glmmTMBm4v12), logLik(glmmTMBm4v12))
timing <- system.time({
  glmmTMBm5v11 <- glmmTMB(WinCloseAction ~ sin((1*omega)*Hour) + Room + (1+TempCadj+SolRad_logadj+OutdoorTempadj|Dwelling),
                          data = datOpen,family = binomial)
});# glmmTMBm5v11$time <- timing
m5 <- c(timing,length(coef(glmmTMBm5v11)$cond$Dwelling), length(coef(glmmTMBm5v11)$cond$Dwelling),AIC(glmmTMBm5v11), BIC(glmmTMBm5v11), logLik(glmmTMBm5v11))
timing <- system.time({
  glmmTMBm6v9 <- glmmTMB(WinCloseAction ~ SolTimer + sin((1*omega)*Hour) + Room + (1+TempCadj+SolRad_logadj+OutdoorTempadj|Dwelling),
                         data = datOpen,family = binomial)
});# glmmTMBm6v9$time <- timing
m6 <- c(timing,length(coef(glmmTMBm6v9)$cond$Dwelling), length(coef(glmmTMBm6v9)$cond$Dwelling),AIC(glmmTMBm6v9), BIC(glmmTMBm6v9), logLik(glmmTMBm6v9))
timing <- system.time({
  glmmTMBm7v14 <- glmmTMB(WinCloseAction ~ Season + SolTimer + sin((1*omega)*Hour) + Room + (1+TempCadj+SolRad_logadj+OutdoorTempadj|Dwelling),
                          data = datOpen,family = binomial)
});# glmmTMBm7v14$time <- timing
m7 <- c(timing,length(coef(glmmTMBm7v14)$cond$Dwelling), length(coef(glmmTMBm7v14)$cond$Dwelling),AIC(glmmTMBm7v14), BIC(glmmTMBm7v14), logLik(glmmTMBm7v14))
timing <- system.time({
  glmmTMBm8v11 <- glmmTMB(WinCloseAction ~ sin((2*omega)*Hour) + Season + SolTimer + sin((1*omega)*Hour) + Room + (1+TempCadj+SolRad_logadj+OutdoorTempadj|Dwelling),
                          data = datOpen,family = binomial)
});# glmmTMBm8v11$time <- timing
m8 <- c(timing,length(coef(glmmTMBm8v11)$cond$Dwelling), length(coef(glmmTMBm8v11)$cond$Dwelling),AIC(glmmTMBm8v11), BIC(glmmTMBm8v11), logLik(glmmTMBm8v11))
timing <- system.time({
  glmmTMBm9v4 <- glmmTMB(WinCloseAction ~ LuxC_log + sin((2*omega)*Hour) + Season + SolTimer + sin((1*omega)*Hour) + Room + (1+TempCadj+SolRad_logadj+OutdoorTempadj|Dwelling),
                         data = datOpen,family = binomial)
});# glmmTMBm9v4$time <- timing
m9 <- c(timing,length(coef(glmmTMBm9v4)$cond$Dwelling), length(coef(glmmTMBm9v4)$cond$Dwelling),AIC(glmmTMBm9v4), BIC(glmmTMBm9v4), logLik(glmmTMBm9v4))
timing <- system.time({
  glmmTMBm10v10 <- glmmTMB(WinCloseAction ~ cos((1*omega)*Hour) + LuxC_log + sin((2*omega)*Hour) + Season + SolTimer + sin((1*omega)*Hour) + Room + (1+TempCadj+SolRad_logadj+OutdoorTempadj|Dwelling),
                           data = datOpen,family = binomial)
});# glmmTMBm10v10$time <- timing
m10 <- c(timing,length(coef(glmmTMBm10v10)$cond$Dwelling), length(coef(glmmTMBm10v10)$cond$Dwelling),AIC(glmmTMBm10v10), BIC(glmmTMBm10v10), logLik(glmmTMBm10v10))
timing <- system.time({
  glmmTMBm11v10 <- glmmTMB(WinCloseAction ~ cos((2*omega)*Hour) + cos((1*omega)*Hour) + LuxC_log + sin((2*omega)*Hour) + Season + SolTimer + sin((1*omega)*Hour) + Room + (1+TempCadj+SolRad_logadj+OutdoorTempadj|Dwelling),
                           data = datOpen,family = binomial)
});# glmmTMBm11v10$time <- timing
m11 <- c(timing,length(coef(glmmTMBm11v10)$cond$Dwelling), length(coef(glmmTMBm11v10)$cond$Dwelling),AIC(glmmTMBm11v10), BIC(glmmTMBm11v10), logLik(glmmTMBm11v10))
timing <- system.time({
  glmmTMBm12v6 <- glmmTMB(WinCloseAction ~ Wind_log + cos((2*omega)*Hour) + cos((1*omega)*Hour) + LuxC_log + sin((2*omega)*Hour) + Season + SolTimer + sin((1*omega)*Hour) + Room + (1+TempCadj+SolRad_logadj+OutdoorTempadj|Dwelling),
                          data = datOpen,family = binomial)
});# glmmTMBm12v6$time <- timing
m12 <- c(timing,length(coef(glmmTMBm12v6)$cond$Dwelling), length(coef(glmmTMBm12v6)$cond$Dwelling),AIC(glmmTMBm12v6), BIC(glmmTMBm12v6), logLik(glmmTMBm12v6))
timing <- system.time({
  glmmTMBm13v5 <- glmmTMB(WinCloseAction ~ OutdoorTemp + Wind_log + cos((2*omega)*Hour) + cos((1*omega)*Hour) + LuxC_log + sin((2*omega)*Hour) + Season + SolTimer + sin((1*omega)*Hour) + Room + (1+TempCadj+SolRad_logadj+OutdoorTempadj|Dwelling),
                          data = datOpen,family = binomial)
});# glmmTMBm13v5$time <- timing
m13 <- c(timing,length(coef(glmmTMBm13v5)$cond$Dwelling), length(coef(glmmTMBm13v5)$cond$Dwelling),AIC(glmmTMBm13v5), BIC(glmmTMBm13v5), logLik(glmmTMBm13v5))
timing <- system.time({
  glmmTMBm14v13 <- glmmTMB(WinCloseAction ~ Weekday + OutdoorTemp + Wind_log + cos((2*omega)*Hour) + cos((1*omega)*Hour) + LuxC_log + sin((2*omega)*Hour) + Season + SolTimer + sin((1*omega)*Hour) + Room + (1+TempCadj+SolRad_logadj+OutdoorTempadj|Dwelling),
                           data = datOpen,family = binomial)
});# glmmTMBm14v13$time <- timing
m14 <- c(timing,length(coef(glmmTMBm14v13)$cond$Dwelling), length(coef(glmmTMBm14v13)$cond$Dwelling),AIC(glmmTMBm14v13), BIC(glmmTMBm14v13), logLik(glmmTMBm14v13))
timing <- system.time({
  glmmTMBm14v13_upd <- glmmTMB(WinCloseAction ~ TempC + SolRad_log + Weekday + OutdoorTemp + Wind_log + cos((2*omega)*Hour) + cos((1*omega)*Hour) + LuxC_log + sin((2*omega)*Hour) + Season + SolTimer + sin((1*omega)*Hour) + Room + (1+TempCadj+SolRad_logadj+OutdoorTempadj|Dwelling),
                               data = datOpen,family = binomial)
});# glmmTMBm14v13_upd$time <- timing
m15 <- c(timing,length(coef(glmmTMBm14v13_upd)$cond$Dwelling), length(coef(glmmTMBm14v13_upd)$cond$Dwelling),AIC(glmmTMBm14v13_upd), BIC(glmmTMBm14v13_upd), logLik(glmmTMBm14v13_upd))
