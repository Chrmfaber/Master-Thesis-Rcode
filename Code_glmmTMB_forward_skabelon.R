#######################################################
####################### MODEL 12 ######################
#######################################################

# timing <- system.time({
#   glmmTMBm12r1 <- glmmTMB(WinOpenAction ~ 1 + RHC + Season + Room + sin((4*omega)*Hour) + sin((3*omega)*Hour) + sin((2*omega)*Hour) + sin((1*omega)*Hour) + (1+OutdoorTempadj+TempCadj+CO2C_logadj+SolRad_logadj|Dwelling),
#                          data = datClosed,family = binomial)
# }); glmmTMBm12r1$time <- timing
# timing <- system.time({
#   glmmTMBm12r2 <- glmmTMB(WinOpenAction ~ 1 + RHC + Season + Room + sin((4*omega)*Hour) + sin((3*omega)*Hour) + sin((2*omega)*Hour) + sin((1*omega)*Hour) + (1+OutdoorTempadj+TempCadj+CO2C_logadj+SolRad_logadj|Dwelling),
#                          data = datClosed,family = binomial)
# }); glmmTMBm12r2$time <- timing
timing <- system.time({
  glmmTMBm12r3 <- glmmTMB(WinOpenAction ~ 1 + RHC + Season + Room + sin((4*omega)*Hour) + sin((3*omega)*Hour) + sin((2*omega)*Hour) + sin((1*omega)*Hour) + (1+RHCadj+OutdoorTempadj+TempCadj+CO2C_logadj+SolRad_logadj|Dwelling),
                         data = datClosed,family = binomial)
}); glmmTMBm12r3$time <- timing
timing <- system.time({
  glmmTMBm12r4 <- glmmTMB(WinOpenAction ~ 1 + RHC + Season + Room + sin((4*omega)*Hour) + sin((3*omega)*Hour) + sin((2*omega)*Hour) + sin((1*omega)*Hour) + (1+LuxC_logadj+OutdoorTempadj+TempCadj+CO2C_logadj+SolRad_logadj|Dwelling),
                         data = datClosed,family = binomial)
}); glmmTMBm12r4$time <- timing
# timing <- system.time({
#   glmmTMBm12r5 <- glmmTMB(WinOpenAction ~ 1 + RHC + Season + Room + sin((4*omega)*Hour) + sin((3*omega)*Hour) + sin((2*omega)*Hour) + sin((1*omega)*Hour) + (1+OutdoorTempadj+TempCadj+CO2C_logadj+SolRad_logadj|Dwelling),
#                          data = datClosed,family = binomial)
# }); glmmTMBm12r5$time <- timing
timing <- system.time({
  glmmTMBm12r6 <- glmmTMB(WinOpenAction ~ 1 + RHC + Season + Room + sin((4*omega)*Hour) + sin((3*omega)*Hour) + sin((2*omega)*Hour) + sin((1*omega)*Hour) + (1+Wind_logadj+OutdoorTempadj+TempCadj+CO2C_logadj+SolRad_logadj|Dwelling),
                         data = datClosed,family = binomial)
}); glmmTMBm12r6$time <- timing
timing <- system.time({
  glmmTMBm12r7 <- glmmTMB(WinOpenAction ~ 1 + RHC + Season + Room + sin((4*omega)*Hour) + sin((3*omega)*Hour) + sin((2*omega)*Hour) + sin((1*omega)*Hour) + (1+OutdoorRHadj+OutdoorTempadj+TempCadj+CO2C_logadj+SolRad_logadj|Dwelling),
                         data = datClosed,family = binomial)
}); glmmTMBm12r7$time <- timing
# timing <- system.time({
#   glmmTMBm12r8 <- glmmTMB(WinOpenAction ~ 1 + RHC + Season + Room + sin((4*omega)*Hour) + sin((3*omega)*Hour) + sin((2*omega)*Hour) + sin((1*omega)*Hour) + (1+OutdoorTempadj+TempCadj+CO2C_logadj+SolRad_logadj|Dwelling),
#                          data = datClosed,family = binomial)
# }); glmmTMBm12r8$time <- timing
timing <- system.time({
  glmmTMBm12r9 <- glmmTMB(WinOpenAction ~ 1 + RHC + Season + Room + sin((4*omega)*Hour) + sin((3*omega)*Hour) + sin((2*omega)*Hour) + sin((1*omega)*Hour) + (1+SolTimeradj+OutdoorTempadj+TempCadj+CO2C_logadj+SolRad_logadj|Dwelling),
                         data = datClosed,family = binomial)
}); glmmTMBm12r9$time <- timing

#######################
timing <- system.time({
  glmmTMBm12v1 <- glmmTMB(WinOpenAction ~ TempC + RHC + Season + Room + sin((4*omega)*Hour) + sin((3*omega)*Hour) + sin((2*omega)*Hour) + sin((1*omega)*Hour) + (1+OutdoorTempadj+TempCadj+CO2C_logadj+SolRad_logadj|Dwelling),
                         data = datClosed,family = binomial)
}); glmmTMBm12v1$time <- timing
timing <- system.time({
  glmmTMBm12v2 <- glmmTMB(WinOpenAction ~ CO2C_log + RHC + Season + Room + sin((4*omega)*Hour) + sin((3*omega)*Hour) + sin((2*omega)*Hour) + sin((1*omega)*Hour) + (1+OutdoorTempadj+TempCadj+CO2C_logadj+SolRad_logadj|Dwelling),
                         data = datClosed,family = binomial)
}); glmmTMBm12v2$time <- timing
# timing <- system.time({
#   glmmTMBm12v3 <- glmmTMB(WinOpenAction ~ RHC + Season + Room + sin((4*omega)*Hour) + sin((3*omega)*Hour) + sin((2*omega)*Hour) + sin((1*omega)*Hour) + (1+OutdoorTempadj+TempCadj+CO2C_logadj+SolRad_logadj|Dwelling),
#                          data = datClosed,family = binomial)
# }); glmmTMBm12v3$time <- timing
timing <- system.time({
  glmmTMBm12v4 <- glmmTMB(WinOpenAction ~ LuxC_log + RHC + Season + Room + sin((4*omega)*Hour) + sin((3*omega)*Hour) + sin((2*omega)*Hour) + sin((1*omega)*Hour) + (1+OutdoorTempadj+TempCadj+CO2C_logadj+SolRad_logadj|Dwelling),
                         data = datClosed,family = binomial)
}); glmmTMBm12v4$time <- timing
timing <- system.time({
  glmmTMBm12v5 <- glmmTMB(WinOpenAction ~ OutdoorTemp + RHC + Season + Room + sin((4*omega)*Hour) + sin((3*omega)*Hour) + sin((2*omega)*Hour) + sin((1*omega)*Hour) + (1+OutdoorTempadj+TempCadj+CO2C_logadj+SolRad_logadj|Dwelling),
                         data = datClosed,family = binomial)
}); glmmTMBm12v5$time <- timing
timing <- system.time({
  glmmTMBm12v6 <- glmmTMB(WinOpenAction ~ Wind_log + RHC + Season + Room + sin((4*omega)*Hour) + sin((3*omega)*Hour) + sin((2*omega)*Hour) + sin((1*omega)*Hour) + (1+OutdoorTempadj+TempCadj+CO2C_logadj+SolRad_logadj|Dwelling),
                         data = datClosed,family = binomial)
}); glmmTMBm12v6$time <- timing
timing <- system.time({
  glmmTMBm12v7 <- glmmTMB(WinOpenAction ~ OutdoorRH + RHC + Season + Room + sin((4*omega)*Hour) + sin((3*omega)*Hour) + sin((2*omega)*Hour) + sin((1*omega)*Hour) + (1+OutdoorTempadj+TempCadj+CO2C_logadj+SolRad_logadj|Dwelling),
                         data = datClosed,family = binomial)
}); glmmTMBm12v7$time <- timing
timing <- system.time({
  glmmTMBm12v8 <- glmmTMB(WinOpenAction ~ SolRad_log + RHC + Season + Room + sin((4*omega)*Hour) + sin((3*omega)*Hour) + sin((2*omega)*Hour) + sin((1*omega)*Hour) + (1+OutdoorTempadj+TempCadj+CO2C_logadj+SolRad_logadj|Dwelling),
                         data = datClosed,family = binomial)
}); glmmTMBm12v8$time <- timing
timing <- system.time({
  glmmTMBm12v9 <- glmmTMB(WinOpenAction ~ SolTimer + RHC + Season + Room + sin((4*omega)*Hour) + sin((3*omega)*Hour) + sin((2*omega)*Hour) + sin((1*omega)*Hour) + (1+OutdoorTempadj+TempCadj+CO2C_logadj+SolRad_logadj|Dwelling),
                         data = datClosed,family = binomial)
}); glmmTMBm12v9$time <- timing
timing <- system.time({
  glmmTMBm12v10 <- glmmTMB(WinOpenAction ~ cos((1*omega)*Hour) + RHC + Season + Room + sin((4*omega)*Hour) + sin((3*omega)*Hour) + sin((2*omega)*Hour) + sin((1*omega)*Hour) + (1+OutdoorTempadj+TempCadj+CO2C_logadj+SolRad_logadj|Dwelling),
                          data = datClosed,family = binomial)
}); glmmTMBm12v10$time <- timing
# timing <- system.time({
#   glmmTMBm12v11 <- glmmTMB(WinOpenAction ~ RHC + Season + Room + sin((4*omega)*Hour) + sin((3*omega)*Hour) + sin((2*omega)*Hour) + sin((1*omega)*Hour) + (1+OutdoorTempadj+TempCadj+CO2C_logadj+SolRad_logadj|Dwelling),
#                           data = datClosed,family = binomial)
# }); glmmTMBm12v11$time <- timing
# timing <- system.time({
#   glmmTMBm12v12 <- glmmTMB(WinOpenAction ~ RHC + Season + Room + sin((4*omega)*Hour) + sin((3*omega)*Hour) + sin((2*omega)*Hour) + sin((1*omega)*Hour) + (1+OutdoorTempadj+TempCadj+CO2C_logadj+SolRad_logadj|Dwelling),
#                           data = datClosed,family = binomial)
# }); glmmTMBm12v12$time <- timing
timing <- system.time({
  glmmTMBm12v13 <- glmmTMB(WinOpenAction ~ Weekday + RHC + Season + Room + sin((4*omega)*Hour) + sin((3*omega)*Hour) + sin((2*omega)*Hour) + sin((1*omega)*Hour) + (1+OutdoorTempadj+TempCadj+CO2C_logadj+SolRad_logadj|Dwelling),
                          data = datClosed,family = binomial)
}); glmmTMBm12v13$time <- timing
# timing <- system.time({
#   glmmTMBm12v14 <- glmmTMB(WinOpenAction ~ RHC + Season + Room + sin((4*omega)*Hour) + sin((3*omega)*Hour) + sin((2*omega)*Hour) + sin((1*omega)*Hour) + (1+OutdoorTempadj+TempCadj+CO2C_logadj+SolRad_logadj|Dwelling),
#                           data = datClosed,family = binomial)
# }); glmmTMBm12v14$time <- timing
timing <- system.time({
  glmmTMBm12v15 <- glmmTMB(WinOpenAction ~ Group + RHC + Season + Room + sin((4*omega)*Hour) + sin((3*omega)*Hour) + sin((2*omega)*Hour) + sin((1*omega)*Hour) + (1+OutdoorTempadj+TempCadj+CO2C_logadj+SolRad_logadj|Dwelling),
                          data = datClosed,family = binomial)
}); glmmTMBm12v15$time <- timing

#######################
#BIC(glmmTMBm12r1)
#BIC(glmmTMBm12r2)
BIC(glmmTMBm12r3)
BIC(glmmTMBm12r4)
#BIC(glmmTMBm12r5)
BIC(glmmTMBm12r6)
BIC(glmmTMBm12r7)
#BIC(glmmTMBm12r8)
BIC(glmmTMBm12r9)

BIC(glmmTMBm12v1)
BIC(glmmTMBm12v2)
#BIC(glmmTMBm12v3)
BIC(glmmTMBm12v4)
BIC(glmmTMBm12v5)
BIC(glmmTMBm12v6)
BIC(glmmTMBm12v7)
BIC(glmmTMBm12v8)
BIC(glmmTMBm12v9)
BIC(glmmTMBm12v10)
#BIC(glmmTMBm12v11)
#BIC(glmmTMBm12v12)
BIC(glmmTMBm12v13)
#BIC(glmmTMBm12v14)
BIC(glmmTMBm12v15)