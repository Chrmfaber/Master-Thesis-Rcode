# inserted #bedst glmer model in forward selection for datClosed nAGQ = 0,data set and save timings
timing <- system.time({
  glmerm1 <- glmer(WinOpenAction ~ 1 + (1|Dwelling),
                   nAGQ = 0,data = datClosed,family = binomial)
});# glmerm1$time <- timing
m0 <- c(timing,length(coef(glmerm1)$cond$Dwelling), length(coef(glmerm1)$cond$Dwelling),AIC(glmerm1), BIC(glmerm1), logLik(glmerm1))
timing <- system.time({
  glmerm1r8 <- glmer(WinOpenAction ~ 1 + (1+SolRad_logadj|Dwelling),
                     nAGQ = 0,data = datClosed,family = binomial)
});# glmerm1r8$time <- timing
m1 <- c(timing,length(coef(glmerm1r8)$cond$Dwelling), length(coef(glmerm1r8)$cond$Dwelling),AIC(glmerm1r8), BIC(glmerm1r8), logLik(glmerm1r8))
timing <- system.time({
  glmerm2r2 <- glmer(WinOpenAction ~ 1 + (1+CO2C_logadj+SolRad_logadj|Dwelling),
                     nAGQ = 0,data = datClosed,family = binomial)
});# glmerm2r2$time <- timing
m2<- c(timing,length(coef(glmerm2r2)$cond$Dwelling), length(coef(glmerm2r2)$cond$Dwelling),AIC(glmerm2r2), BIC(glmerm2r2), logLik(glmerm2r2))
timing <- system.time({
  glmerm3v11 <- glmer(WinOpenAction ~ sin((1*omega)*Hour) + (1+CO2C_logadj+SolRad_logadj|Dwelling),
                      nAGQ = 0,data = datClosed,family = binomial)
});# glmerm3v11$time <- timing
m3 <- c(timing,length(coef(glmerm3v11)$cond$Dwelling), length(coef(glmerm3v11)$cond$Dwelling),AIC(glmerm3v11), BIC(glmerm3v11), logLik(glmerm3v11))
timing <- system.time({
  glmerm4r1 <- glmer(WinOpenAction ~ 1 + sin((1*omega)*Hour) + (1+TempCadj+CO2C_logadj+SolRad_logadj|Dwelling),
                     nAGQ = 0,data = datClosed,family = binomial)
});# glmerm4r1$time <- timing
m4 <- c(timing,length(coef(glmerm4r1)$cond$Dwelling), length(coef(glmerm4r1)$cond$Dwelling),AIC(glmerm4r1), BIC(glmerm4r1), logLik(glmerm4r1))
timing <- system.time({
  glmerm5r5 <- glmer(WinOpenAction ~ 1 + sin((1*omega)*Hour) + (1+OutdoorTempadj+TempCadj+CO2C_logadj+SolRad_logadj|Dwelling),
                     nAGQ = 0,data = datClosed,family = binomial)
});# glmerm5r5$time <- timing
m5 <- c(timing,length(coef(glmerm5r5)$cond$Dwelling), length(coef(glmerm5r5)$cond$Dwelling),AIC(glmerm5r5), BIC(glmerm5r5), logLik(glmerm5r5))
timing <- system.time({
  glmerm6v11 <- glmer(WinOpenAction ~ sin((2*omega)*Hour) + sin((1*omega)*Hour) + (1+OutdoorTempadj+TempCadj+CO2C_logadj+SolRad_logadj|Dwelling),
                      nAGQ = 0,data = datClosed,family = binomial)
});# glmerm6v11$time <- timing
m6 <- c(timing,length(coef(glmerm6v11)$cond$Dwelling), length(coef(glmerm6v11)$cond$Dwelling),AIC(glmerm6v11), BIC(glmerm6v11), logLik(glmerm6v11))
timing <- system.time({
  glmerm7v11 <- glmer(WinOpenAction ~ sin((3*omega)*Hour) + sin((2*omega)*Hour) + sin((1*omega)*Hour) + (1+OutdoorTempadj+TempCadj+CO2C_logadj+SolRad_logadj|Dwelling),
                      nAGQ = 0,data = datClosed,family = binomial)
});# glmerm7v11$time <- timing
m7 <- c(timing,length(coef(glmerm7v11)$cond$Dwelling), length(coef(glmerm7v11)$cond$Dwelling),AIC(glmerm7v11), BIC(glmerm7v11), logLik(glmerm7v11))
timing <- system.time({
  glmerm8v11 <- glmer(WinOpenAction ~ sin((4*omega)*Hour) + sin((3*omega)*Hour) + sin((2*omega)*Hour) + sin((1*omega)*Hour) + (1+OutdoorTempadj+TempCadj+CO2C_logadj+SolRad_logadj|Dwelling),
                      nAGQ = 0,data = datClosed,family = binomial)
});# glmerm8v11$time <- timing
m8 <- c(timing,length(coef(glmerm8v11)$cond$Dwelling), length(coef(glmerm8v11)$cond$Dwelling),AIC(glmerm8v11), BIC(glmerm8v11), logLik(glmerm8v11))
timing <- system.time({
  glmerm9v12 <- glmer(WinOpenAction ~ Room + sin((4*omega)*Hour) + sin((3*omega)*Hour) + sin((2*omega)*Hour) + sin((1*omega)*Hour) + (1+OutdoorTempadj+TempCadj+CO2C_logadj+SolRad_logadj|Dwelling),
                      nAGQ = 0,data = datClosed,family = binomial)
});# glmerm9v12$time <- timing
m9 <- c(timing,length(coef(glmerm9v12)$cond$Dwelling), length(coef(glmerm9v12)$cond$Dwelling),AIC(glmerm9v12), BIC(glmerm9v12), logLik(glmerm9v12))
timing <- system.time({
  glmerm10v14 <- glmer(WinOpenAction ~ Season + Room + sin((4*omega)*Hour) + sin((3*omega)*Hour) + sin((2*omega)*Hour) + sin((1*omega)*Hour) + (1+OutdoorTempadj+TempCadj+CO2C_logadj+SolRad_logadj|Dwelling),
                       nAGQ = 0,data = datClosed,family = binomial)
});# glmerm10v14$time <- timing
m10 <- c(timing,length(coef(glmerm10v14)$cond$Dwelling), length(coef(glmerm10v14)$cond$Dwelling),AIC(glmerm10v14), BIC(glmerm10v14), logLik(glmerm10v14))
timing <- system.time({
  glmerm11v3 <- glmer(WinOpenAction ~ RHC + Season + Room + sin((4*omega)*Hour) + sin((3*omega)*Hour) + sin((2*omega)*Hour) + sin((1*omega)*Hour) + (1+OutdoorTempadj+TempCadj+CO2C_logadj+SolRad_logadj|Dwelling),
                      nAGQ = 0,data = datClosed,family = binomial)
});# glmerm11v3$time <- timing
m11 <- c(timing,length(coef(glmerm11v3)$cond$Dwelling), length(coef(glmerm11v3)$cond$Dwelling),AIC(glmerm11v3), BIC(glmerm11v3), logLik(glmerm11v3))
timing <- system.time({
  glmerm11v3_upd <- glmer(WinOpenAction ~ OutdoorTemp + TempC + CO2C_log + SolRad_log
                          + RHC + Season + Room + sin((4*omega)*Hour) + sin((3*omega)*Hour) + sin((2*omega)*Hour) + sin((1*omega)*Hour) + (1+OutdoorTempadj+TempCadj+CO2C_logadj+SolRad_logadj|Dwelling),
                          nAGQ = 0,data = datClosed,family = binomial)
});# glmerm11v3_upd$time <- timing
m12 <- c(timing,length(coef(glmerm11v3_upd)$cond$Dwelling), length(coef(glmerm11v3_upd)$cond$Dwelling),AIC(glmerm11v3_upd), BIC(glmerm11v3_upd), logLik(glmerm11v3_upd))
