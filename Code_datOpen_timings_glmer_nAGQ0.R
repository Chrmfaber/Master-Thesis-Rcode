# inserted #bedst glmer model in forward selection for datOpen data set and save timings
timing <- system.time({
  glmerm1 <- glmer(WinCloseAction ~ 1 + (1|Dwelling),
                   nAGQ = 0,data = datOpen,family = binomial)
});# glmerm1$time <- timing
m0 <- c(timing,length(coef(glmerm1)$cond$Dwelling), length(coef(glmerm1)$cond$Dwelling),AIC(glmerm1), BIC(glmerm1), logLik(glmerm1))
timing <- system.time({
  glmerm1r5 <- glmer(WinCloseAction ~ 1 + (1+OutdoorTempadj|Dwelling),
                     nAGQ = 0,data = datOpen,family = binomial)
});# glmerm1r5$time <- timing
m1 <- c(timing,length(coef(glmerm1r5)$cond$Dwelling), length(coef(glmerm1r5)$cond$Dwelling),AIC(glmerm1r5), BIC(glmerm1r5), logLik(glmerm1r5))
timing <- system.time({
  glmerm2r8 <- glmer(WinCloseAction ~ 1 + (1+SolRad_logadj+OutdoorTempadj|Dwelling),
                     nAGQ = 0,data = datOpen,family = binomial)
});# glmerm2r8$time <- timing
m2 <- c(timing,length(coef(glmerm2r8)$cond$Dwelling), length(coef(glmerm2r8)$cond$Dwelling),AIC(glmerm2r8), BIC(glmerm2r8), logLik(glmerm2r8))
timing <- system.time({
  glmerm3r1 <- glmer(WinCloseAction ~ 1 + (1+TempCadj+SolRad_logadj+SolRad_logadj+OutdoorTempadj|Dwelling),
                     nAGQ = 0,data = datOpen,family = binomial)
});# glmerm3r1$time <- timing
m3 <- c(timing,length(coef(glmerm3r1)$cond$Dwelling), length(coef(glmerm3r1)$cond$Dwelling),AIC(glmerm3r1), BIC(glmerm3r1), logLik(glmerm3r1))
timing <- system.time({
  glmerm4v12 <- glmer(WinCloseAction ~ Room + (1+TempCadj+SolRad_logadj+OutdoorTempadj|Dwelling),
                      nAGQ = 0,data = datOpen,family = binomial)
});# glmerm4v12$time <- timing
m4 <- c(timing,length(coef(glmerm4v12)$cond$Dwelling), length(coef(glmerm4v12)$cond$Dwelling),AIC(glmerm4v12), BIC(glmerm4v12), logLik(glmerm4v12))
timing <- system.time({
  glmerm5v11 <- glmer(WinCloseAction ~ sin((1*omega)*Hour) + Room + (1+TempCadj+SolRad_logadj+OutdoorTempadj|Dwelling),
                      nAGQ = 0,data = datOpen,family = binomial)
});# glmerm5v11$time <- timing
m5 <- c(timing,length(coef(glmerm5v11)$cond$Dwelling), length(coef(glmerm5v11)$cond$Dwelling),AIC(glmerm5v11), BIC(glmerm5v11), logLik(glmerm5v11))
timing <- system.time({
  glmerm6v9 <- glmer(WinCloseAction ~ SolTimer + sin((1*omega)*Hour) + Room + (1+TempCadj+SolRad_logadj+OutdoorTempadj|Dwelling),
                     nAGQ = 0,data = datOpen,family = binomial)
});# glmerm6v9$time <- timing
m6 <- c(timing,length(coef(glmerm6v9)$cond$Dwelling), length(coef(glmerm6v9)$cond$Dwelling),AIC(glmerm6v9), BIC(glmerm6v9), logLik(glmerm6v9))
timing <- system.time({
  glmerm7v14 <- glmer(WinCloseAction ~ Season + SolTimer + sin((1*omega)*Hour) + Room + (1+TempCadj+SolRad_logadj+OutdoorTempadj|Dwelling),
                      nAGQ = 0,data = datOpen,family = binomial)
});# glmerm7v14$time <- timing
m7 <- c(timing,length(coef(glmerm7v14)$cond$Dwelling), length(coef(glmerm7v14)$cond$Dwelling),AIC(glmerm7v14), BIC(glmerm7v14), logLik(glmerm7v14))
timing <- system.time({
  glmerm8v11 <- glmer(WinCloseAction ~ sin((2*omega)*Hour) + Season + SolTimer + sin((1*omega)*Hour) + Room + (1+TempCadj+SolRad_logadj+OutdoorTempadj|Dwelling),
                      nAGQ = 0,data = datOpen,family = binomial)
});# glmerm8v11$time <- timing
m8 <- c(timing,length(coef(glmerm8v11)$cond$Dwelling), length(coef(glmerm8v11)$cond$Dwelling),AIC(glmerm8v11), BIC(glmerm8v11), logLik(glmerm8v11))
timing <- system.time({
  glmerm9v4 <- glmer(WinCloseAction ~ LuxC_log + sin((2*omega)*Hour) + Season + SolTimer + sin((1*omega)*Hour) + Room + (1+TempCadj+SolRad_logadj+OutdoorTempadj|Dwelling),
                     nAGQ = 0,data = datOpen,family = binomial)
});# glmerm9v4$time <- timing
m9 <- c(timing,length(coef(glmerm9v4)$cond$Dwelling), length(coef(glmerm9v4)$cond$Dwelling),AIC(glmerm9v4), BIC(glmerm9v4), logLik(glmerm9v4))
timing <- system.time({
  glmerm10v10 <- glmer(WinCloseAction ~ cos((1*omega)*Hour) + LuxC_log + sin((2*omega)*Hour) + Season + SolTimer + sin((1*omega)*Hour) + Room + (1+TempCadj+SolRad_logadj+OutdoorTempadj|Dwelling),
                       nAGQ = 0,data = datOpen,family = binomial)
});# glmerm10v10$time <- timing
m10 <- c(timing,length(coef(glmerm10v10)$cond$Dwelling), length(coef(glmerm10v10)$cond$Dwelling),AIC(glmerm10v10), BIC(glmerm10v10), logLik(glmerm10v10))
timing <- system.time({
  glmerm11v10 <- glmer(WinCloseAction ~ cos((2*omega)*Hour) + cos((1*omega)*Hour) + LuxC_log + sin((2*omega)*Hour) + Season + SolTimer + sin((1*omega)*Hour) + Room + (1+TempCadj+SolRad_logadj+OutdoorTempadj|Dwelling),
                       nAGQ = 0,data = datOpen,family = binomial)
});# glmerm11v10$time <- timing
m11 <- c(timing,length(coef(glmerm11v10)$cond$Dwelling), length(coef(glmerm11v10)$cond$Dwelling),AIC(glmerm11v10), BIC(glmerm11v10), logLik(glmerm11v10))
timing <- system.time({
  glmerm12v6 <- glmer(WinCloseAction ~ Wind_log + cos((2*omega)*Hour) + cos((1*omega)*Hour) + LuxC_log + sin((2*omega)*Hour) + Season + SolTimer + sin((1*omega)*Hour) + Room + (1+TempCadj+SolRad_logadj+OutdoorTempadj|Dwelling),
                      nAGQ = 0,data = datOpen,family = binomial)
});# glmerm12v6$time <- timing
m12 <- c(timing,length(coef(glmerm12v6)$cond$Dwelling), length(coef(glmerm12v6)$cond$Dwelling),AIC(glmerm12v6), BIC(glmerm12v6), logLik(glmerm12v6))
timing <- system.time({
  glmerm13v5 <- glmer(WinCloseAction ~ OutdoorTemp + Wind_log + cos((2*omega)*Hour) + cos((1*omega)*Hour) + LuxC_log + sin((2*omega)*Hour) + Season + SolTimer + sin((1*omega)*Hour) + Room + (1+TempCadj+SolRad_logadj+OutdoorTempadj|Dwelling),
                      nAGQ = 0,data = datOpen,family = binomial)
});# glmerm13v5$time <- timing
m13 <- c(timing,length(coef(glmerm13v5)$cond$Dwelling), length(coef(glmerm13v5)$cond$Dwelling),AIC(glmerm13v5), BIC(glmerm13v5), logLik(glmerm13v5))
timing <- system.time({
  glmerm14v13 <- glmer(WinCloseAction ~ Weekday + OutdoorTemp + Wind_log + cos((2*omega)*Hour) + cos((1*omega)*Hour) + LuxC_log + sin((2*omega)*Hour) + Season + SolTimer + sin((1*omega)*Hour) + Room + (1+TempCadj+SolRad_logadj+OutdoorTempadj|Dwelling),
                       nAGQ = 0,data = datOpen,family = binomial)
});# glmerm14v13$time <- timing
m14 <- c(timing,length(coef(glmerm14v13)$cond$Dwelling), length(coef(glmerm14v13)$cond$Dwelling),AIC(glmerm14v13), BIC(glmerm14v13), logLik(glmerm14v13))
timing <- system.time({
  glmerm14v13_upd <- glmer(WinCloseAction ~ TempC + SolRad_log + Weekday + OutdoorTemp + Wind_log + cos((2*omega)*Hour) + cos((1*omega)*Hour) + LuxC_log + sin((2*omega)*Hour) + Season + SolTimer + sin((1*omega)*Hour) + Room + (1+TempCadj+SolRad_logadj+OutdoorTempadj|Dwelling),
                           nAGQ = 0,data = datOpen,family = binomial)
});# glmerm14v13_upd$time <- timing
m15 <- c(timing,length(coef(glmerm14v13_upd)$cond$Dwelling), length(coef(glmerm14v13_upd)$cond$Dwelling),AIC(glmerm14v13_upd), BIC(glmerm14v13_upd), logLik(glmerm14v13_upd))
