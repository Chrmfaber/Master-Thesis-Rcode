#######################################################
####################### MODEL 0 #######################
#######################################################

timing <- system.time({
  glmmTMBm0 <- glmmTMB(WinOpenAction ~ 1,
                       data = datClosed,family = binomial)
}); glmmTMBm0$time <- timing

#######################
BIC(glmmTMBm0)

#######################################################
####################### MODEL 1 #######################
#######################################################

timing <- system.time({
  glmmTMBm1 <- glmmTMB(WinOpenAction ~ 1 + (1|Dwelling),
                       data = datClosed,family = binomial)
}); glmmTMBm1$time <- timing

#######################
BIC(glmmTMBm1)


#######################
#######################
#######################

timing <- system.time({
  glmmTMBm1r1 <- glmmTMB(WinOpenAction ~ 1 + (1+TempCadj|Dwelling),
                         data = datClosed,family = binomial)
}); glmmTMBm1r1$time <- timing
timing <- system.time({
  glmmTMBm1r2 <- glmmTMB(WinOpenAction ~ 1 + (1+CO2C_logadj|Dwelling),
                         data = datClosed,family = binomial)
}); glmmTMBm1r2$time <- timing
timing <- system.time({
  glmmTMBm1r3 <- glmmTMB(WinOpenAction ~ 1 + (1+RHCadj|Dwelling),
                         data = datClosed,family = binomial)
}); glmmTMBm1r3$time <- timing
timing <- system.time({
  glmmTMBm1r4 <- glmmTMB(WinOpenAction ~ 1 + (1+LuxC_logadj|Dwelling),
                         data = datClosed,family = binomial)
}); glmmTMBm1r4$time <- timing
timing <- system.time({
  glmmTMBm1r5 <- glmmTMB(WinOpenAction ~ 1 + (1+OutdoorTempadj|Dwelling),
                         data = datClosed,family = binomial)
}); glmmTMBm1r5$time <- timing
timing <- system.time({
  glmmTMBm1r6 <- glmmTMB(WinOpenAction ~ 1 + (1+Wind_logadj|Dwelling),
                         data = datClosed,family = binomial)
}); glmmTMBm1r6$time <- timing
timing <- system.time({
  glmmTMBm1r7 <- glmmTMB(WinOpenAction ~ 1 + (1+OutdoorRHadj|Dwelling),
                         data = datClosed,family = binomial)
}); glmmTMBm1r7$time <- timing
timing <- system.time({
  glmmTMBm1r8 <- glmmTMB(WinOpenAction ~ 1 + (1+SolRad_logadj|Dwelling),
                         data = datClosed,family = binomial)
}); glmmTMBm1r8$time <- timing
timing <- system.time({
  glmmTMBm1r9 <- glmmTMB(WinOpenAction ~ 1 + (1+SolTimeradj|Dwelling),
                         data = datClosed,family = binomial)
}); glmmTMBm1r9$time <- timing

#######################
timing <- system.time({
  glmmTMBm1v1 <- glmmTMB(WinOpenAction ~ TempC + (1|Dwelling),
                         data = datClosed,family = binomial)
}); glmmTMBm1v1$time <- timing
timing <- system.time({
  glmmTMBm1v2 <- glmmTMB(WinOpenAction ~ CO2C_log + (1|Dwelling),
                         data = datClosed,family = binomial)
}); glmmTMBm1v2$time <- timing
timing <- system.time({
  glmmTMBm1v3 <- glmmTMB(WinOpenAction ~ RHC + (1|Dwelling),
                         data = datClosed,family = binomial)
}); glmmTMBm1v3$time <- timing
timing <- system.time({
  glmmTMBm1v4 <- glmmTMB(WinOpenAction ~ LuxC_log + (1|Dwelling),
                         data = datClosed,family = binomial)
}); glmmTMBm1v4$time <- timing
timing <- system.time({
  glmmTMBm1v5 <- glmmTMB(WinOpenAction ~ OutdoorTemp + (1|Dwelling),
                         data = datClosed,family = binomial)
}); glmmTMBm1v5$time <- timing
timing <- system.time({
  glmmTMBm1v6 <- glmmTMB(WinOpenAction ~ Wind_log + (1|Dwelling),
                         data = datClosed,family = binomial)
}); glmmTMBm1v6$time <- timing
timing <- system.time({
  glmmTMBm1v7 <- glmmTMB(WinOpenAction ~ OutdoorRH + (1|Dwelling),
                         data = datClosed,family = binomial)
}); glmmTMBm1v7$time <- timing
timing <- system.time({
  glmmTMBm1v8 <- glmmTMB(WinOpenAction ~ SolRad_log + (1|Dwelling),
                         data = datClosed,family = binomial)
}); glmmTMBm1v8$time <- timing
timing <- system.time({
  glmmTMBm1v9 <- glmmTMB(WinOpenAction ~ SolTimer + (1|Dwelling),
                         data = datClosed,family = binomial)
}); glmmTMBm1v9$time <- timing
timing <- system.time({
  glmmTMBm1v10 <- glmmTMB(WinOpenAction ~ cos((1*omega)*Hour) + (1|Dwelling),
                          data = datClosed,family = binomial)
}); glmmTMBm1v10$time <- timing
timing <- system.time({
  glmmTMBm1v11 <- glmmTMB(WinOpenAction ~ sin((1*omega)*Hour) + (1|Dwelling),
                          data = datClosed,family = binomial)
}); glmmTMBm1v11$time <- timing
timing <- system.time({
  glmmTMBm1v12 <- glmmTMB(WinOpenAction ~ Room + (1|Dwelling),
                          data = datClosed,family = binomial)
}); glmmTMBm1v12$time <- timing
timing <- system.time({
  glmmTMBm1v13 <- glmmTMB(WinOpenAction ~ Weekday + (1|Dwelling),
                          data = datClosed,family = binomial)
}); glmmTMBm1v13$time <- timing
timing <- system.time({
  glmmTMBm1v14 <- glmmTMB(WinOpenAction ~ Season + (1|Dwelling),
                          data = datClosed,family = binomial)
}); glmmTMBm1v14$time <- timing
timing <- system.time({
  glmmTMBm1v15 <- glmmTMB(WinOpenAction ~ Group + (1|Dwelling),
                          data = datClosed,family = binomial)
}); glmmTMBm1v15$time <- timing

#######################
BIC(glmmTMBm1r1)
BIC(glmmTMBm1r2)
BIC(glmmTMBm1r3)
BIC(glmmTMBm1r4)
BIC(glmmTMBm1r5)
BIC(glmmTMBm1r6)
BIC(glmmTMBm1r7)
BIC(glmmTMBm1r8) #bedst
BIC(glmmTMBm1r9)

BIC(glmmTMBm1v1)
BIC(glmmTMBm1v2)
BIC(glmmTMBm1v3)
BIC(glmmTMBm1v4)
BIC(glmmTMBm1v5)
BIC(glmmTMBm1v6)
BIC(glmmTMBm1v7)
BIC(glmmTMBm1v8)
BIC(glmmTMBm1v9)
BIC(glmmTMBm1v10)
BIC(glmmTMBm1v11)
BIC(glmmTMBm1v12)
BIC(glmmTMBm1v13)
BIC(glmmTMBm1v14)
BIC(glmmTMBm1v15)

#######################################################
####################### MODEL 2 #######################
#######################################################

timing <- system.time({
  glmmTMBm2r1 <- glmmTMB(WinOpenAction ~ 1 + (1+TempCadj+SolRad_logadj|Dwelling),
                         data = datClosed,family = binomial)
}); glmmTMBm2r1$time <- timing
timing <- system.time({
  glmmTMBm2r2 <- glmmTMB(WinOpenAction ~ 1 + (1+CO2C_logadj+SolRad_logadj|Dwelling),
                         data = datClosed,family = binomial)
}); glmmTMBm2r2$time <- timing
timing <- system.time({
  glmmTMBm2r3 <- glmmTMB(WinOpenAction ~ 1 + (1+RHCadj+SolRad_logadj|Dwelling),
                         data = datClosed,family = binomial)
}); glmmTMBm2r3$time <- timing
timing <- system.time({
  glmmTMBm2r4 <- glmmTMB(WinOpenAction ~ 1 + (1+LuxC_logadj+SolRad_logadj|Dwelling),
                         data = datClosed,family = binomial)
}); glmmTMBm2r4$time <- timing
timing <- system.time({
  glmmTMBm2r5 <- glmmTMB(WinOpenAction ~ 1 + (1+OutdoorTempadj+SolRad_logadj|Dwelling),
                         data = datClosed,family = binomial)
}); glmmTMBm2r5$time <- timing
timing <- system.time({
  glmmTMBm2r6 <- glmmTMB(WinOpenAction ~ 1 + (1+Wind_logadj+SolRad_logadj|Dwelling),
                         data = datClosed,family = binomial)
}); glmmTMBm2r6$time <- timing
timing <- system.time({
  glmmTMBm2r7 <- glmmTMB(WinOpenAction ~ 1 + (1+OutdoorRHadj+SolRad_logadj|Dwelling),
                         data = datClosed,family = binomial)
}); glmmTMBm2r7$time <- timing
# timing <- system.time({
#   glmmTMBm2r8 <- glmmTMB(WinOpenAction ~ 1 + (1+SolRad_logadj|Dwelling),
#                          data = datClosed,family = binomial)
# }); glmmTMBm2r8$time <- timing
timing <- system.time({
  glmmTMBm2r9 <- glmmTMB(WinOpenAction ~ 1 + (1+SolTimeradj+SolRad_logadj|Dwelling),
                         data = datClosed,family = binomial)
}); glmmTMBm2r9$time <- timing

#######################
timing <- system.time({
  glmmTMBm2v1 <- glmmTMB(WinOpenAction ~ TempC + (1+SolRad_logadj|Dwelling),
                         data = datClosed,family = binomial)
}); glmmTMBm2v1$time <- timing
timing <- system.time({
  glmmTMBm2v2 <- glmmTMB(WinOpenAction ~ CO2C_log + (1+SolRad_logadj|Dwelling),
                         data = datClosed,family = binomial)
}); glmmTMBm2v2$time <- timing
timing <- system.time({
  glmmTMBm2v3 <- glmmTMB(WinOpenAction ~ RHC + (1+SolRad_logadj|Dwelling),
                         data = datClosed,family = binomial)
}); glmmTMBm2v3$time <- timing
timing <- system.time({
  glmmTMBm2v4 <- glmmTMB(WinOpenAction ~ LuxC_log + (1+SolRad_logadj|Dwelling),
                         data = datClosed,family = binomial)
}); glmmTMBm2v4$time <- timing
timing <- system.time({
  glmmTMBm2v5 <- glmmTMB(WinOpenAction ~ OutdoorTemp + (1+SolRad_logadj|Dwelling),
                         data = datClosed,family = binomial)
}); glmmTMBm2v5$time <- timing
timing <- system.time({
  glmmTMBm2v6 <- glmmTMB(WinOpenAction ~ Wind_log + (1+SolRad_logadj|Dwelling),
                         data = datClosed,family = binomial)
}); glmmTMBm2v6$time <- timing
timing <- system.time({
  glmmTMBm2v7 <- glmmTMB(WinOpenAction ~ OutdoorRH + (1+SolRad_logadj|Dwelling),
                         data = datClosed,family = binomial)
}); glmmTMBm2v7$time <- timing
timing <- system.time({
  glmmTMBm2v8 <- glmmTMB(WinOpenAction ~ SolRad_log + (1+SolRad_logadj|Dwelling),
                         data = datClosed,family = binomial)
}); glmmTMBm2v8$time <- timing
timing <- system.time({
  glmmTMBm2v9 <- glmmTMB(WinOpenAction ~ SolTimer + (1+SolRad_logadj|Dwelling),
                         data = datClosed,family = binomial)
}); glmmTMBm2v9$time <- timing
timing <- system.time({
  glmmTMBm2v10 <- glmmTMB(WinOpenAction ~ cos((1*omega)*Hour) + (1+SolRad_logadj|Dwelling),
                          data = datClosed,family = binomial)
}); glmmTMBm2v10$time <- timing
timing <- system.time({
  glmmTMBm2v11 <- glmmTMB(WinOpenAction ~ sin((1*omega)*Hour) + (1+SolRad_logadj|Dwelling),
                          data = datClosed,family = binomial)
}); glmmTMBm2v11$time <- timing
timing <- system.time({
  glmmTMBm2v12 <- glmmTMB(WinOpenAction ~ Room + (1+SolRad_logadj|Dwelling),
                          data = datClosed,family = binomial)
}); glmmTMBm2v12$time <- timing
timing <- system.time({
  glmmTMBm2v13 <- glmmTMB(WinOpenAction ~ Weekday + (1+SolRad_logadj|Dwelling),
                          data = datClosed,family = binomial)
}); glmmTMBm2v13$time <- timing
timing <- system.time({
  glmmTMBm2v14 <- glmmTMB(WinOpenAction ~ Season + (1+SolRad_logadj|Dwelling),
                          data = datClosed,family = binomial)
}); glmmTMBm2v14$time <- timing
timing <- system.time({
  glmmTMBm2v15 <- glmmTMB(WinOpenAction ~ Group + (1+SolRad_logadj|Dwelling),
                          data = datClosed,family = binomial)
}); glmmTMBm2v15$time <- timing

#######################
BIC(glmmTMBm2r1)
BIC(glmmTMBm2r2) #bedst
BIC(glmmTMBm2r3)
BIC(glmmTMBm2r4)
BIC(glmmTMBm2r5)
BIC(glmmTMBm2r6)
BIC(glmmTMBm2r7)
#BIC(glmmTMBm2r8)
BIC(glmmTMBm2r9)

BIC(glmmTMBm2v1)
BIC(glmmTMBm2v2)
BIC(glmmTMBm2v3)
BIC(glmmTMBm2v4)
BIC(glmmTMBm2v5)
BIC(glmmTMBm2v6)
BIC(glmmTMBm2v7)
BIC(glmmTMBm2v8)
BIC(glmmTMBm2v9)
BIC(glmmTMBm2v10)
BIC(glmmTMBm2v11)
BIC(glmmTMBm2v12)
BIC(glmmTMBm2v13)
BIC(glmmTMBm2v14)
BIC(glmmTMBm2v15)

#######################################################
####################### MODEL 3 #######################
#######################################################

timing <- system.time({
  glmmTMBm3r1 <- glmmTMB(WinOpenAction ~ 1 + (1+TempCadj+CO2C_logadj+SolRad_logadj|Dwelling),
                         data = datClosed,family = binomial)
}); glmmTMBm3r1$time <- timing
# timing <- system.time({
#   glmmTMBm3r2 <- glmmTMB(WinOpenAction ~ 1 + (1+CO2C_logadj+SolRad_logadj|Dwelling),
#                          data = datClosed,family = binomial)
# }); glmmTMBm3r2$time <- timing
timing <- system.time({
  glmmTMBm3r3 <- glmmTMB(WinOpenAction ~ 1 + (1+RHCadj+CO2C_logadj+SolRad_logadj|Dwelling),
                         data = datClosed,family = binomial)
}); glmmTMBm3r3$time <- timing
timing <- system.time({
  glmmTMBm3r4 <- glmmTMB(WinOpenAction ~ 1 + (1+LuxC_logadj+CO2C_logadj+SolRad_logadj|Dwelling),
                         data = datClosed,family = binomial)
}); glmmTMBm3r4$time <- timing
timing <- system.time({
  glmmTMBm3r5 <- glmmTMB(WinOpenAction ~ 1 + (1+OutdoorTempadj+CO2C_logadj+SolRad_logadj|Dwelling),
                         data = datClosed,family = binomial)
}); glmmTMBm3r5$time <- timing
timing <- system.time({
  glmmTMBm3r6 <- glmmTMB(WinOpenAction ~ 1 + (1+Wind_logadj+CO2C_logadj+SolRad_logadj|Dwelling),
                         data = datClosed,family = binomial)
}); glmmTMBm3r6$time <- timing
timing <- system.time({
  glmmTMBm3r7 <- glmmTMB(WinOpenAction ~ 1 + (1+OutdoorRHadj+CO2C_logadj+SolRad_logadj|Dwelling),
                         data = datClosed,family = binomial)
}); glmmTMBm3r7$time <- timing
# timing <- system.time({
#   glmmTMBm3r8 <- glmmTMB(WinOpenAction ~ 1 + (1+CO2C_logadj+SolRad_logadj|Dwelling),
#                          data = datClosed,family = binomial)
# }); glmmTMBm3r8$time <- timing
timing <- system.time({
  glmmTMBm3r9 <- glmmTMB(WinOpenAction ~ 1 + (1+SolTimeradj+CO2C_logadj+SolRad_logadj|Dwelling),
                         data = datClosed,family = binomial)
}); glmmTMBm3r9$time <- timing

#######################
timing <- system.time({
  glmmTMBm3v1 <- glmmTMB(WinOpenAction ~ TempC + (1+CO2C_logadj+SolRad_logadj|Dwelling),
                         data = datClosed,family = binomial)
}); glmmTMBm3v1$time <- timing
timing <- system.time({
  glmmTMBm3v2 <- glmmTMB(WinOpenAction ~ CO2C_log + (1+CO2C_logadj+SolRad_logadj|Dwelling),
                         data = datClosed,family = binomial)
}); glmmTMBm3v2$time <- timing
timing <- system.time({
  glmmTMBm3v3 <- glmmTMB(WinOpenAction ~ RHC + (1+CO2C_logadj+SolRad_logadj|Dwelling),
                         data = datClosed,family = binomial)
}); glmmTMBm3v3$time <- timing
timing <- system.time({
  glmmTMBm3v4 <- glmmTMB(WinOpenAction ~ LuxC_log + (1+CO2C_logadj+SolRad_logadj|Dwelling),
                         data = datClosed,family = binomial)
}); glmmTMBm3v4$time <- timing
timing <- system.time({
  glmmTMBm3v5 <- glmmTMB(WinOpenAction ~ OutdoorTemp + (1+CO2C_logadj+SolRad_logadj|Dwelling),
                         data = datClosed,family = binomial)
}); glmmTMBm3v5$time <- timing
timing <- system.time({
  glmmTMBm3v6 <- glmmTMB(WinOpenAction ~ Wind_log + (1+CO2C_logadj+SolRad_logadj|Dwelling),
                         data = datClosed,family = binomial)
}); glmmTMBm3v6$time <- timing
timing <- system.time({
  glmmTMBm3v7 <- glmmTMB(WinOpenAction ~ OutdoorRH + (1+CO2C_logadj+SolRad_logadj|Dwelling),
                         data = datClosed,family = binomial)
}); glmmTMBm3v7$time <- timing
timing <- system.time({
  glmmTMBm3v8 <- glmmTMB(WinOpenAction ~ SolRad_log + (1+CO2C_logadj+SolRad_logadj|Dwelling),
                         data = datClosed,family = binomial)
}); glmmTMBm3v8$time <- timing
timing <- system.time({
  glmmTMBm3v9 <- glmmTMB(WinOpenAction ~ SolTimer + (1+CO2C_logadj+SolRad_logadj|Dwelling),
                         data = datClosed,family = binomial)
}); glmmTMBm3v9$time <- timing
timing <- system.time({
  glmmTMBm3v10 <- glmmTMB(WinOpenAction ~ cos((1*omega)*Hour) + (1+CO2C_logadj+SolRad_logadj|Dwelling),
                          data = datClosed,family = binomial)
}); glmmTMBm3v10$time <- timing
timing <- system.time({
  glmmTMBm3v11 <- glmmTMB(WinOpenAction ~ sin((1*omega)*Hour) + (1+CO2C_logadj+SolRad_logadj|Dwelling),
                          data = datClosed,family = binomial)
}); glmmTMBm3v11$time <- timing
timing <- system.time({
  glmmTMBm3v12 <- glmmTMB(WinOpenAction ~ Room + (1+CO2C_logadj+SolRad_logadj|Dwelling),
                          data = datClosed,family = binomial)
}); glmmTMBm3v12$time <- timing
timing <- system.time({
  glmmTMBm3v13 <- glmmTMB(WinOpenAction ~ Weekday + (1+CO2C_logadj+SolRad_logadj|Dwelling),
                          data = datClosed,family = binomial)
}); glmmTMBm3v13$time <- timing
timing <- system.time({
  glmmTMBm3v14 <- glmmTMB(WinOpenAction ~ Season + (1+CO2C_logadj+SolRad_logadj|Dwelling),
                          data = datClosed,family = binomial)
}); glmmTMBm3v14$time <- timing
timing <- system.time({
  glmmTMBm3v15 <- glmmTMB(WinOpenAction ~ Group + (1+CO2C_logadj+SolRad_logadj|Dwelling),
                          data = datClosed,family = binomial)
}); glmmTMBm3v15$time <- timing

#######################
BIC(glmmTMBm3r1)
#BIC(glmmTMBm3r2)
BIC(glmmTMBm3r3)
BIC(glmmTMBm3r4)
BIC(glmmTMBm3r5)
BIC(glmmTMBm3r6)
BIC(glmmTMBm3r7)
#BIC(glmmTMBm3r8)
BIC(glmmTMBm3r9)

BIC(glmmTMBm3v1)
BIC(glmmTMBm3v2)
BIC(glmmTMBm3v3)
BIC(glmmTMBm3v4)
BIC(glmmTMBm3v5)
BIC(glmmTMBm3v6)
BIC(glmmTMBm3v7)
BIC(glmmTMBm3v8)
BIC(glmmTMBm3v9)
BIC(glmmTMBm3v10)
BIC(glmmTMBm3v11) #bedst
BIC(glmmTMBm3v12)
BIC(glmmTMBm3v13)
BIC(glmmTMBm3v14)
BIC(glmmTMBm3v15)

#######################################################
####################### MODEL 4 #######################
#######################################################

timing <- system.time({
  glmmTMBm4r1 <- glmmTMB(WinOpenAction ~ 1 + sin((1*omega)*Hour) + (1+TempCadj+CO2C_logadj+SolRad_logadj|Dwelling),
                         data = datClosed,family = binomial)
}); glmmTMBm4r1$time <- timing
# timing <- system.time({
#   glmmTMBm4r2 <- glmmTMB(WinOpenAction ~ 1 + sin((1*omega)*Hour) + (1+CO2C_logadj+SolRad_logadj|Dwelling),
#                          data = datClosed,family = binomial)
# }); glmmTMBm4r2$time <- timing
timing <- system.time({
  glmmTMBm4r3 <- glmmTMB(WinOpenAction ~ 1 + sin((1*omega)*Hour) + (1+RHCadj+CO2C_logadj+SolRad_logadj|Dwelling),
                         data = datClosed,family = binomial)
}); glmmTMBm4r3$time <- timing
timing <- system.time({
  glmmTMBm4r4 <- glmmTMB(WinOpenAction ~ 1 + sin((1*omega)*Hour) + (1+LuxC_logadj+CO2C_logadj+SolRad_logadj|Dwelling),
                         data = datClosed,family = binomial)
}); glmmTMBm4r4$time <- timing
timing <- system.time({
  glmmTMBm4r5 <- glmmTMB(WinOpenAction ~ 1 + sin((1*omega)*Hour) + (1+OutdoorTempadj+CO2C_logadj+SolRad_logadj|Dwelling),
                         data = datClosed,family = binomial)
}); glmmTMBm4r5$time <- timing
timing <- system.time({
  glmmTMBm4r6 <- glmmTMB(WinOpenAction ~ 1 + sin((1*omega)*Hour) + (1+Wind_logadj+CO2C_logadj+SolRad_logadj|Dwelling),
                         data = datClosed,family = binomial)
}); glmmTMBm4r6$time <- timing
timing <- system.time({
  glmmTMBm4r7 <- glmmTMB(WinOpenAction ~ 1 + sin((1*omega)*Hour) + (1+OutdoorRHadj+CO2C_logadj+SolRad_logadj|Dwelling),
                         data = datClosed,family = binomial)
}); glmmTMBm4r7$time <- timing
# timing <- system.time({
#   glmmTMBm4r8 <- glmmTMB(WinOpenAction ~ 1 + sin((1*omega)*Hour) + (1+CO2C_logadj+SolRad_logadj|Dwelling),
#                          data = datClosed,family = binomial)
# }); glmmTMBm4r8$time <- timing
timing <- system.time({
  glmmTMBm4r9 <- glmmTMB(WinOpenAction ~ 1 + sin((1*omega)*Hour) + (1+SolTimeradj+CO2C_logadj+SolRad_logadj|Dwelling),
                         data = datClosed,family = binomial)
}); glmmTMBm4r9$time <- timing

#######################
timing <- system.time({
  glmmTMBm4v1 <- glmmTMB(WinOpenAction ~ TempC + sin((1*omega)*Hour) + (1+CO2C_logadj+SolRad_logadj|Dwelling),
                         data = datClosed,family = binomial)
}); glmmTMBm4v1$time <- timing
timing <- system.time({
  glmmTMBm4v2 <- glmmTMB(WinOpenAction ~ CO2C_log + sin((1*omega)*Hour) + (1+CO2C_logadj+SolRad_logadj|Dwelling),
                         data = datClosed,family = binomial)
}); glmmTMBm4v2$time <- timing
timing <- system.time({
  glmmTMBm4v3 <- glmmTMB(WinOpenAction ~ RHC + sin((1*omega)*Hour) + (1+CO2C_logadj+SolRad_logadj|Dwelling),
                         data = datClosed,family = binomial)
}); glmmTMBm4v3$time <- timing
timing <- system.time({
  glmmTMBm4v4 <- glmmTMB(WinOpenAction ~ LuxC_log + sin((1*omega)*Hour) + (1+CO2C_logadj+SolRad_logadj|Dwelling),
                         data = datClosed,family = binomial)
}); glmmTMBm4v4$time <- timing
timing <- system.time({
  glmmTMBm4v5 <- glmmTMB(WinOpenAction ~ OutdoorTemp + sin((1*omega)*Hour) + (1+CO2C_logadj+SolRad_logadj|Dwelling),
                         data = datClosed,family = binomial)
}); glmmTMBm4v5$time <- timing
timing <- system.time({
  glmmTMBm4v6 <- glmmTMB(WinOpenAction ~ Wind_log + sin((1*omega)*Hour) + (1+CO2C_logadj+SolRad_logadj|Dwelling),
                         data = datClosed,family = binomial)
}); glmmTMBm4v6$time <- timing
timing <- system.time({
  glmmTMBm4v7 <- glmmTMB(WinOpenAction ~ OutdoorRH + sin((1*omega)*Hour) + (1+CO2C_logadj+SolRad_logadj|Dwelling),
                         data = datClosed,family = binomial)
}); glmmTMBm4v7$time <- timing
timing <- system.time({
  glmmTMBm4v8 <- glmmTMB(WinOpenAction ~ SolRad_log + sin((1*omega)*Hour) + (1+CO2C_logadj+SolRad_logadj|Dwelling),
                         data = datClosed,family = binomial)
}); glmmTMBm4v8$time <- timing
timing <- system.time({
  glmmTMBm4v9 <- glmmTMB(WinOpenAction ~ SolTimer + sin((1*omega)*Hour) + (1+CO2C_logadj+SolRad_logadj|Dwelling),
                         data = datClosed,family = binomial)
}); glmmTMBm4v9$time <- timing
timing <- system.time({
  glmmTMBm4v10 <- glmmTMB(WinOpenAction ~ cos((1*omega)*Hour) + sin((1*omega)*Hour) + (1+CO2C_logadj+SolRad_logadj|Dwelling),
                          data = datClosed,family = binomial)
}); glmmTMBm4v10$time <- timing
timing <- system.time({
  glmmTMBm4v11 <- glmmTMB(WinOpenAction ~ sin((2*omega)*Hour) + sin((1*omega)*Hour) + (1+CO2C_logadj+SolRad_logadj|Dwelling),
                          data = datClosed,family = binomial)
}); glmmTMBm4v11$time <- timing
timing <- system.time({
  glmmTMBm4v12 <- glmmTMB(WinOpenAction ~ Room + sin((1*omega)*Hour) + (1+CO2C_logadj+SolRad_logadj|Dwelling),
                          data = datClosed,family = binomial)
}); glmmTMBm4v12$time <- timing
timing <- system.time({
  glmmTMBm4v13 <- glmmTMB(WinOpenAction ~ Weekday + sin((1*omega)*Hour) + (1+CO2C_logadj+SolRad_logadj|Dwelling),
                          data = datClosed,family = binomial)
}); glmmTMBm4v13$time <- timing
timing <- system.time({
  glmmTMBm4v14 <- glmmTMB(WinOpenAction ~ Season + sin((1*omega)*Hour) + (1+CO2C_logadj+SolRad_logadj|Dwelling),
                          data = datClosed,family = binomial)
}); glmmTMBm4v14$time <- timing
timing <- system.time({
  glmmTMBm4v15 <- glmmTMB(WinOpenAction ~ Group + sin((1*omega)*Hour) + (1+CO2C_logadj+SolRad_logadj|Dwelling),
                          data = datClosed,family = binomial)
}); glmmTMBm4v15$time <- timing

#######################
BIC(glmmTMBm4r1) #bedst
#BIC(glmmTMBm4r2)
BIC(glmmTMBm4r3)
BIC(glmmTMBm4r4)
BIC(glmmTMBm4r5)
BIC(glmmTMBm4r6)
BIC(glmmTMBm4r7)
#BIC(glmmTMBm4r8)
BIC(glmmTMBm4r9)

BIC(glmmTMBm4v1)
BIC(glmmTMBm4v2)
BIC(glmmTMBm4v3)
BIC(glmmTMBm4v4)
BIC(glmmTMBm4v5)
BIC(glmmTMBm4v6)
BIC(glmmTMBm4v7)
BIC(glmmTMBm4v8)
BIC(glmmTMBm4v9)
BIC(glmmTMBm4v10)
BIC(glmmTMBm4v11)
BIC(glmmTMBm4v12)
BIC(glmmTMBm4v13)
BIC(glmmTMBm4v14)
BIC(glmmTMBm4v15)

#######################################################
####################### MODEL 5 #######################
#######################################################

# timing <- system.time({
#   glmmTMBm5r1 <- glmmTMB(WinOpenAction ~ 1 + sin((1*omega)*Hour) + (1+TempCadj+CO2C_logadj+SolRad_logadj|Dwelling),
#                          data = datClosed,family = binomial)
# }); glmmTMBm5r1$time <- timing
# timing <- system.time({
#   glmmTMBm5r2 <- glmmTMB(WinOpenAction ~ 1 + sin((1*omega)*Hour) + (1+TempCadj+CO2C_logadj+SolRad_logadj|Dwelling),
#                          data = datClosed,family = binomial)
# }); glmmTMBm5r2$time <- timing
timing <- system.time({
  glmmTMBm5r3 <- glmmTMB(WinOpenAction ~ 1 + sin((1*omega)*Hour) + (1+RHCadj+TempCadj+CO2C_logadj+SolRad_logadj|Dwelling),
                         data = datClosed,family = binomial)
}); glmmTMBm5r3$time <- timing
timing <- system.time({
  glmmTMBm5r4 <- glmmTMB(WinOpenAction ~ 1 + sin((1*omega)*Hour) + (1+LuxC_logadj+TempCadj+CO2C_logadj+SolRad_logadj|Dwelling),
                         data = datClosed,family = binomial)
}); glmmTMBm5r4$time <- timing
timing <- system.time({
  glmmTMBm5r5 <- glmmTMB(WinOpenAction ~ 1 + sin((1*omega)*Hour) + (1+OutdoorTempadj+TempCadj+CO2C_logadj+SolRad_logadj|Dwelling),
                         data = datClosed,family = binomial)
}); glmmTMBm5r5$time <- timing
timing <- system.time({
  glmmTMBm5r6 <- glmmTMB(WinOpenAction ~ 1 + sin((1*omega)*Hour) + (1+Wind_logadj+TempCadj+CO2C_logadj+SolRad_logadj|Dwelling),
                         data = datClosed,family = binomial)
}); glmmTMBm5r6$time <- timing
timing <- system.time({
  glmmTMBm5r7 <- glmmTMB(WinOpenAction ~ 1 + sin((1*omega)*Hour) + (1+OutdoorRHadj+TempCadj+CO2C_logadj+SolRad_logadj|Dwelling),
                         data = datClosed,family = binomial)
}); glmmTMBm5r7$time <- timing
# timing <- system.time({
#   glmmTMBm5r8 <- glmmTMB(WinOpenAction ~ 1 + sin((1*omega)*Hour) + (1+TempCadj+CO2C_logadj+SolRad_logadj|Dwelling),
#                          data = datClosed,family = binomial)
# }); glmmTMBm5r8$time <- timing
timing <- system.time({
  glmmTMBm5r9 <- glmmTMB(WinOpenAction ~ 1 + sin((1*omega)*Hour) + (1+SolTimeradj+TempCadj+CO2C_logadj+SolRad_logadj|Dwelling),
                         data = datClosed,family = binomial)
}); glmmTMBm5r9$time <- timing

#######################
timing <- system.time({
  glmmTMBm5v1 <- glmmTMB(WinOpenAction ~ TempC + sin((1*omega)*Hour) + (1+TempCadj+CO2C_logadj+SolRad_logadj|Dwelling),
                         data = datClosed,family = binomial)
}); glmmTMBm5v1$time <- timing
timing <- system.time({
  glmmTMBm5v2 <- glmmTMB(WinOpenAction ~ CO2C_log + sin((1*omega)*Hour) + (1+TempCadj+CO2C_logadj+SolRad_logadj|Dwelling),
                         data = datClosed,family = binomial)
}); glmmTMBm5v2$time <- timing
timing <- system.time({
  glmmTMBm5v3 <- glmmTMB(WinOpenAction ~ RHC + sin((1*omega)*Hour) + (1+TempCadj+CO2C_logadj+SolRad_logadj|Dwelling),
                         data = datClosed,family = binomial)
}); glmmTMBm5v3$time <- timing
timing <- system.time({
  glmmTMBm5v4 <- glmmTMB(WinOpenAction ~ LuxC_log + sin((1*omega)*Hour) + (1+TempCadj+CO2C_logadj+SolRad_logadj|Dwelling),
                         data = datClosed,family = binomial)
}); glmmTMBm5v4$time <- timing
timing <- system.time({
  glmmTMBm5v5 <- glmmTMB(WinOpenAction ~ OutdoorTemp + sin((1*omega)*Hour) + (1+TempCadj+CO2C_logadj+SolRad_logadj|Dwelling),
                         data = datClosed,family = binomial)
}); glmmTMBm5v5$time <- timing
timing <- system.time({
  glmmTMBm5v6 <- glmmTMB(WinOpenAction ~ Wind_log + sin((1*omega)*Hour) + (1+TempCadj+CO2C_logadj+SolRad_logadj|Dwelling),
                         data = datClosed,family = binomial)
}); glmmTMBm5v6$time <- timing
timing <- system.time({
  glmmTMBm5v7 <- glmmTMB(WinOpenAction ~ OutdoorRH + sin((1*omega)*Hour) + (1+TempCadj+CO2C_logadj+SolRad_logadj|Dwelling),
                         data = datClosed,family = binomial)
}); glmmTMBm5v7$time <- timing
timing <- system.time({
  glmmTMBm5v8 <- glmmTMB(WinOpenAction ~ SolRad_log + sin((1*omega)*Hour) + (1+TempCadj+CO2C_logadj+SolRad_logadj|Dwelling),
                         data = datClosed,family = binomial)
}); glmmTMBm5v8$time <- timing
timing <- system.time({
  glmmTMBm5v9 <- glmmTMB(WinOpenAction ~ SolTimer + sin((1*omega)*Hour) + (1+TempCadj+CO2C_logadj+SolRad_logadj|Dwelling),
                         data = datClosed,family = binomial)
}); glmmTMBm5v9$time <- timing
timing <- system.time({
  glmmTMBm5v10 <- glmmTMB(WinOpenAction ~ cos((1*omega)*Hour) + sin((1*omega)*Hour) + (1+TempCadj+CO2C_logadj+SolRad_logadj|Dwelling),
                          data = datClosed,family = binomial)
}); glmmTMBm5v10$time <- timing
timing <- system.time({
  glmmTMBm5v11 <- glmmTMB(WinOpenAction ~ sin((2*omega)*Hour) + sin((1*omega)*Hour) + (1+TempCadj+CO2C_logadj+SolRad_logadj|Dwelling),
                          data = datClosed,family = binomial)
}); glmmTMBm5v11$time <- timing
timing <- system.time({
  glmmTMBm5v12 <- glmmTMB(WinOpenAction ~ Room + sin((1*omega)*Hour) + (1+TempCadj+CO2C_logadj+SolRad_logadj|Dwelling),
                          data = datClosed,family = binomial)
}); glmmTMBm5v12$time <- timing
timing <- system.time({
  glmmTMBm5v13 <- glmmTMB(WinOpenAction ~ Weekday + sin((1*omega)*Hour) + (1+TempCadj+CO2C_logadj+SolRad_logadj|Dwelling),
                          data = datClosed,family = binomial)
}); glmmTMBm5v13$time <- timing
timing <- system.time({
  glmmTMBm5v14 <- glmmTMB(WinOpenAction ~ Season + sin((1*omega)*Hour) + (1+TempCadj+CO2C_logadj+SolRad_logadj|Dwelling),
                          data = datClosed,family = binomial)
}); glmmTMBm5v14$time <- timing
timing <- system.time({
  glmmTMBm5v15 <- glmmTMB(WinOpenAction ~ Group + sin((1*omega)*Hour) + (1+TempCadj+CO2C_logadj+SolRad_logadj|Dwelling),
                          data = datClosed,family = binomial)
}); glmmTMBm5v15$time <- timing

#######################
#BIC(glmmTMBm5r1)
#BIC(glmmTMBm5r2)
BIC(glmmTMBm5r3)
BIC(glmmTMBm5r4)
BIC(glmmTMBm5r5) #bedst
BIC(glmmTMBm5r6)
BIC(glmmTMBm5r7)
#BIC(glmmTMBm5r8)
BIC(glmmTMBm5r9)

BIC(glmmTMBm5v1)
BIC(glmmTMBm5v2)
BIC(glmmTMBm5v3)
BIC(glmmTMBm5v4)
BIC(glmmTMBm5v5)
BIC(glmmTMBm5v6)
BIC(glmmTMBm5v7)
BIC(glmmTMBm5v8)
BIC(glmmTMBm5v9)
BIC(glmmTMBm5v10)
BIC(glmmTMBm5v11)
BIC(glmmTMBm5v12)
BIC(glmmTMBm5v13)
BIC(glmmTMBm5v14)
BIC(glmmTMBm5v15)

#######################################################
####################### MODEL 6 #######################
#######################################################

# timing <- system.time({
#   glmmTMBm6r1 <- glmmTMB(WinOpenAction ~ 1 + sin((1*omega)*Hour) + (1+OutdoorTempadj+TempCadj+CO2C_logadj+SolRad_logadj|Dwelling),
#                          data = datClosed,family = binomial)
# }); glmmTMBm6r1$time <- timing
# timing <- system.time({
#   glmmTMBm6r2 <- glmmTMB(WinOpenAction ~ 1 + sin((1*omega)*Hour) + (1+OutdoorTempadj+TempCadj+CO2C_logadj+SolRad_logadj|Dwelling),
#                          data = datClosed,family = binomial)
# }); glmmTMBm6r2$time <- timing
timing <- system.time({
  glmmTMBm6r3 <- glmmTMB(WinOpenAction ~ 1 + sin((1*omega)*Hour) + (1+RHCadj+OutdoorTempadj+TempCadj+CO2C_logadj+SolRad_logadj|Dwelling),
                         data = datClosed,family = binomial)
}); glmmTMBm6r3$time <- timing
timing <- system.time({
  glmmTMBm6r4 <- glmmTMB(WinOpenAction ~ 1 + sin((1*omega)*Hour) + (1+LuxC_logadj+OutdoorTempadj+TempCadj+CO2C_logadj+SolRad_logadj|Dwelling),
                         data = datClosed,family = binomial)
}); glmmTMBm6r4$time <- timing
# timing <- system.time({
#   glmmTMBm6r5 <- glmmTMB(WinOpenAction ~ 1 + sin((1*omega)*Hour) + (1+OutdoorTempadj+TempCadj+CO2C_logadj+SolRad_logadj|Dwelling),
#                          data = datClosed,family = binomial)
# }); glmmTMBm6r5$time <- timing
timing <- system.time({
  glmmTMBm6r6 <- glmmTMB(WinOpenAction ~ 1 + sin((1*omega)*Hour) + (1+Wind_logadj+OutdoorTempadj+TempCadj+CO2C_logadj+SolRad_logadj|Dwelling),
                         data = datClosed,family = binomial)
}); glmmTMBm6r6$time <- timing
timing <- system.time({
  glmmTMBm6r7 <- glmmTMB(WinOpenAction ~ 1 + sin((1*omega)*Hour) + (1+OutdoorRHadj+OutdoorTempadj+TempCadj+CO2C_logadj+SolRad_logadj|Dwelling),
                         data = datClosed,family = binomial)
}); glmmTMBm6r7$time <- timing
# timing <- system.time({
#   glmmTMBm6r8 <- glmmTMB(WinOpenAction ~ 1 + sin((1*omega)*Hour) + (1+OutdoorTempadj+TempCadj+CO2C_logadj+SolRad_logadj|Dwelling),
#                          data = datClosed,family = binomial)
# }); glmmTMBm6r8$time <- timing
timing <- system.time({
  glmmTMBm6r9 <- glmmTMB(WinOpenAction ~ 1 + sin((1*omega)*Hour) + (1+SolTimeradj+OutdoorTempadj+TempCadj+CO2C_logadj+SolRad_logadj|Dwelling),
                         data = datClosed,family = binomial)
}); glmmTMBm6r9$time <- timing

#######################
timing <- system.time({
  glmmTMBm6v1 <- glmmTMB(WinOpenAction ~ TempC + sin((1*omega)*Hour) + (1+OutdoorTempadj+TempCadj+CO2C_logadj+SolRad_logadj|Dwelling),
                         data = datClosed,family = binomial)
}); glmmTMBm6v1$time <- timing
timing <- system.time({
  glmmTMBm6v2 <- glmmTMB(WinOpenAction ~ CO2C_log + sin((1*omega)*Hour) + (1+OutdoorTempadj+TempCadj+CO2C_logadj+SolRad_logadj|Dwelling),
                         data = datClosed,family = binomial)
}); glmmTMBm6v2$time <- timing
timing <- system.time({
  glmmTMBm6v3 <- glmmTMB(WinOpenAction ~ RHC + sin((1*omega)*Hour) + (1+OutdoorTempadj+TempCadj+CO2C_logadj+SolRad_logadj|Dwelling),
                         data = datClosed,family = binomial)
}); glmmTMBm6v3$time <- timing
timing <- system.time({
  glmmTMBm6v4 <- glmmTMB(WinOpenAction ~ LuxC_log + sin((1*omega)*Hour) + (1+OutdoorTempadj+TempCadj+CO2C_logadj+SolRad_logadj|Dwelling),
                         data = datClosed,family = binomial)
}); glmmTMBm6v4$time <- timing
timing <- system.time({
  glmmTMBm6v5 <- glmmTMB(WinOpenAction ~ OutdoorTemp + sin((1*omega)*Hour) + (1+OutdoorTempadj+TempCadj+CO2C_logadj+SolRad_logadj|Dwelling),
                         data = datClosed,family = binomial)
}); glmmTMBm6v5$time <- timing
timing <- system.time({
  glmmTMBm6v6 <- glmmTMB(WinOpenAction ~ Wind_log + sin((1*omega)*Hour) + (1+OutdoorTempadj+TempCadj+CO2C_logadj+SolRad_logadj|Dwelling),
                         data = datClosed,family = binomial)
}); glmmTMBm6v6$time <- timing
timing <- system.time({
  glmmTMBm6v7 <- glmmTMB(WinOpenAction ~ OutdoorRH + sin((1*omega)*Hour) + (1+OutdoorTempadj+TempCadj+CO2C_logadj+SolRad_logadj|Dwelling),
                         data = datClosed,family = binomial)
}); glmmTMBm6v7$time <- timing
timing <- system.time({
  glmmTMBm6v8 <- glmmTMB(WinOpenAction ~ SolRad_log + sin((1*omega)*Hour) + (1+OutdoorTempadj+TempCadj+CO2C_logadj+SolRad_logadj|Dwelling),
                         data = datClosed,family = binomial)
}); glmmTMBm6v8$time <- timing
timing <- system.time({
  glmmTMBm6v9 <- glmmTMB(WinOpenAction ~ SolTimer + sin((1*omega)*Hour) + (1+OutdoorTempadj+TempCadj+CO2C_logadj+SolRad_logadj|Dwelling),
                         data = datClosed,family = binomial)
}); glmmTMBm6v9$time <- timing
timing <- system.time({
  glmmTMBm6v10 <- glmmTMB(WinOpenAction ~ cos((1*omega)*Hour) + sin((1*omega)*Hour) + (1+OutdoorTempadj+TempCadj+CO2C_logadj+SolRad_logadj|Dwelling),
                          data = datClosed,family = binomial)
}); glmmTMBm6v10$time <- timing
timing <- system.time({
  glmmTMBm6v11 <- glmmTMB(WinOpenAction ~ sin((2*omega)*Hour) + sin((1*omega)*Hour) + (1+OutdoorTempadj+TempCadj+CO2C_logadj+SolRad_logadj|Dwelling),
                          data = datClosed,family = binomial)
}); glmmTMBm6v11$time <- timing
timing <- system.time({
  glmmTMBm6v12 <- glmmTMB(WinOpenAction ~ Room + sin((1*omega)*Hour) + (1+OutdoorTempadj+TempCadj+CO2C_logadj+SolRad_logadj|Dwelling),
                          data = datClosed,family = binomial)
}); glmmTMBm6v12$time <- timing
timing <- system.time({
  glmmTMBm6v13 <- glmmTMB(WinOpenAction ~ Weekday + sin((1*omega)*Hour) + (1+OutdoorTempadj+TempCadj+CO2C_logadj+SolRad_logadj|Dwelling),
                          data = datClosed,family = binomial)
}); glmmTMBm6v13$time <- timing
timing <- system.time({
  glmmTMBm6v14 <- glmmTMB(WinOpenAction ~ Season + sin((1*omega)*Hour) + (1+OutdoorTempadj+TempCadj+CO2C_logadj+SolRad_logadj|Dwelling),
                          data = datClosed,family = binomial)
}); glmmTMBm6v14$time <- timing
timing <- system.time({
  glmmTMBm6v15 <- glmmTMB(WinOpenAction ~ Group + sin((1*omega)*Hour) + (1+OutdoorTempadj+TempCadj+CO2C_logadj+SolRad_logadj|Dwelling),
                          data = datClosed,family = binomial)
}); glmmTMBm6v15$time <- timing

#######################
#BIC(glmmTMBm6r1)
#BIC(glmmTMBm6r2)
BIC(glmmTMBm6r3)
BIC(glmmTMBm6r4)
#BIC(glmmTMBm6r5)
BIC(glmmTMBm6r6)
BIC(glmmTMBm6r7)
#BIC(glmmTMBm6r8)
BIC(glmmTMBm6r9)

BIC(glmmTMBm6v1)
BIC(glmmTMBm6v2)
BIC(glmmTMBm6v3)
BIC(glmmTMBm6v4)
BIC(glmmTMBm6v5)
BIC(glmmTMBm6v6)
BIC(glmmTMBm6v7)
BIC(glmmTMBm6v8)
BIC(glmmTMBm6v9)
BIC(glmmTMBm6v10)
BIC(glmmTMBm6v11) #bedst
BIC(glmmTMBm6v12)
BIC(glmmTMBm6v13)
BIC(glmmTMBm6v14)
BIC(glmmTMBm6v15)

#######################################################
####################### MODEL 7 #######################
#######################################################

# timing <- system.time({
#   glmmTMBm7r1 <- glmmTMB(WinOpenAction ~ 1 + sin((2*omega)*Hour) + sin((1*omega)*Hour) + (1+OutdoorTempadj+TempCadj+CO2C_logadj+SolRad_logadj|Dwelling),
#                          data = datClosed,family = binomial)
# }); glmmTMBm7r1$time <- timing
# timing <- system.time({
#   glmmTMBm7r2 <- glmmTMB(WinOpenAction ~ 1 + sin((2*omega)*Hour) + sin((1*omega)*Hour) + (1+OutdoorTempadj+TempCadj+CO2C_logadj+SolRad_logadj|Dwelling),
#                          data = datClosed,family = binomial)
# }); glmmTMBm7r2$time <- timing
timing <- system.time({
  glmmTMBm7r3 <- glmmTMB(WinOpenAction ~ 1 + sin((2*omega)*Hour) + sin((1*omega)*Hour) + (1+RHCadj+OutdoorTempadj+TempCadj+CO2C_logadj+SolRad_logadj|Dwelling),
                         data = datClosed,family = binomial)
}); glmmTMBm7r3$time <- timing
timing <- system.time({
  glmmTMBm7r4 <- glmmTMB(WinOpenAction ~ 1 + sin((2*omega)*Hour) + sin((1*omega)*Hour) + (1+LuxC_logadj+OutdoorTempadj+TempCadj+CO2C_logadj+SolRad_logadj|Dwelling),
                         data = datClosed,family = binomial)
}); glmmTMBm7r4$time <- timing
# timing <- system.time({
#   glmmTMBm7r5 <- glmmTMB(WinOpenAction ~ 1 + sin((2*omega)*Hour) + sin((1*omega)*Hour) + (1+OutdoorTempadj+TempCadj+CO2C_logadj+SolRad_logadj|Dwelling),
#                          data = datClosed,family = binomial)
# }); glmmTMBm7r5$time <- timing
timing <- system.time({
  glmmTMBm7r6 <- glmmTMB(WinOpenAction ~ 1 + sin((2*omega)*Hour) + sin((1*omega)*Hour) + (1+Wind_logadj+OutdoorTempadj+TempCadj+CO2C_logadj+SolRad_logadj|Dwelling),
                         data = datClosed,family = binomial)
}); glmmTMBm7r6$time <- timing
timing <- system.time({
  glmmTMBm7r7 <- glmmTMB(WinOpenAction ~ 1 + sin((2*omega)*Hour) + sin((1*omega)*Hour) + (1+OutdoorRHadj+OutdoorTempadj+TempCadj+CO2C_logadj+SolRad_logadj|Dwelling),
                         data = datClosed,family = binomial)
}); glmmTMBm7r7$time <- timing
# timing <- system.time({
#   glmmTMBm7r8 <- glmmTMB(WinOpenAction ~ 1 + sin((2*omega)*Hour) + sin((1*omega)*Hour) + (1+OutdoorTempadj+TempCadj+CO2C_logadj+SolRad_logadj|Dwelling),
#                          data = datClosed,family = binomial)
# }); glmmTMBm7r8$time <- timing
timing <- system.time({
  glmmTMBm7r9 <- glmmTMB(WinOpenAction ~ 1 + sin((2*omega)*Hour) + sin((1*omega)*Hour) + (1+SolTimeradj+OutdoorTempadj+TempCadj+CO2C_logadj+SolRad_logadj|Dwelling),
                         data = datClosed,family = binomial)
}); glmmTMBm7r9$time <- timing

#######################
timing <- system.time({
  glmmTMBm7v1 <- glmmTMB(WinOpenAction ~ TempC + sin((2*omega)*Hour) + sin((1*omega)*Hour) + (1+OutdoorTempadj+TempCadj+CO2C_logadj+SolRad_logadj|Dwelling),
                         data = datClosed,family = binomial)
}); glmmTMBm7v1$time <- timing
timing <- system.time({
  glmmTMBm7v2 <- glmmTMB(WinOpenAction ~ CO2C_log + sin((2*omega)*Hour) + sin((1*omega)*Hour) + (1+OutdoorTempadj+TempCadj+CO2C_logadj+SolRad_logadj|Dwelling),
                         data = datClosed,family = binomial)
}); glmmTMBm7v2$time <- timing
timing <- system.time({
  glmmTMBm7v3 <- glmmTMB(WinOpenAction ~ RHC + sin((2*omega)*Hour) + sin((1*omega)*Hour) + (1+OutdoorTempadj+TempCadj+CO2C_logadj+SolRad_logadj|Dwelling),
                         data = datClosed,family = binomial)
}); glmmTMBm7v3$time <- timing
timing <- system.time({
  glmmTMBm7v4 <- glmmTMB(WinOpenAction ~ LuxC_log + sin((2*omega)*Hour) + sin((1*omega)*Hour) + (1+OutdoorTempadj+TempCadj+CO2C_logadj+SolRad_logadj|Dwelling),
                         data = datClosed,family = binomial)
}); glmmTMBm7v4$time <- timing
timing <- system.time({
  glmmTMBm7v5 <- glmmTMB(WinOpenAction ~ OutdoorTemp + sin((2*omega)*Hour) + sin((1*omega)*Hour) + (1+OutdoorTempadj+TempCadj+CO2C_logadj+SolRad_logadj|Dwelling),
                         data = datClosed,family = binomial)
}); glmmTMBm7v5$time <- timing
timing <- system.time({
  glmmTMBm7v6 <- glmmTMB(WinOpenAction ~ Wind_log + sin((2*omega)*Hour) + sin((1*omega)*Hour) + (1+OutdoorTempadj+TempCadj+CO2C_logadj+SolRad_logadj|Dwelling),
                         data = datClosed,family = binomial)
}); glmmTMBm7v6$time <- timing
timing <- system.time({
  glmmTMBm7v7 <- glmmTMB(WinOpenAction ~ OutdoorRH + sin((2*omega)*Hour) + sin((1*omega)*Hour) + (1+OutdoorTempadj+TempCadj+CO2C_logadj+SolRad_logadj|Dwelling),
                         data = datClosed,family = binomial)
}); glmmTMBm7v7$time <- timing
timing <- system.time({
  glmmTMBm7v8 <- glmmTMB(WinOpenAction ~ SolRad_log + sin((2*omega)*Hour) + sin((1*omega)*Hour) + (1+OutdoorTempadj+TempCadj+CO2C_logadj+SolRad_logadj|Dwelling),
                         data = datClosed,family = binomial)
}); glmmTMBm7v8$time <- timing
timing <- system.time({
  glmmTMBm7v9 <- glmmTMB(WinOpenAction ~ SolTimer + sin((2*omega)*Hour) + sin((1*omega)*Hour) + (1+OutdoorTempadj+TempCadj+CO2C_logadj+SolRad_logadj|Dwelling),
                         data = datClosed,family = binomial)
}); glmmTMBm7v9$time <- timing
timing <- system.time({
  glmmTMBm7v10 <- glmmTMB(WinOpenAction ~ cos((1*omega)*Hour) + sin((2*omega)*Hour) + sin((1*omega)*Hour) + (1+OutdoorTempadj+TempCadj+CO2C_logadj+SolRad_logadj|Dwelling),
                          data = datClosed,family = binomial)
}); glmmTMBm7v10$time <- timing
timing <- system.time({
  glmmTMBm7v11 <- glmmTMB(WinOpenAction ~ sin((3*omega)*Hour) + sin((2*omega)*Hour) + sin((1*omega)*Hour) + (1+OutdoorTempadj+TempCadj+CO2C_logadj+SolRad_logadj|Dwelling),
                          data = datClosed,family = binomial)
}); glmmTMBm7v11$time <- timing
timing <- system.time({
  glmmTMBm7v12 <- glmmTMB(WinOpenAction ~ Room + sin((2*omega)*Hour) + sin((1*omega)*Hour) + (1+OutdoorTempadj+TempCadj+CO2C_logadj+SolRad_logadj|Dwelling),
                          data = datClosed,family = binomial)
}); glmmTMBm7v12$time <- timing
timing <- system.time({
  glmmTMBm7v13 <- glmmTMB(WinOpenAction ~ Weekday + sin((2*omega)*Hour) + sin((1*omega)*Hour) + (1+OutdoorTempadj+TempCadj+CO2C_logadj+SolRad_logadj|Dwelling),
                          data = datClosed,family = binomial)
}); glmmTMBm7v13$time <- timing
timing <- system.time({
  glmmTMBm7v14 <- glmmTMB(WinOpenAction ~ Season + sin((2*omega)*Hour) + sin((1*omega)*Hour) + (1+OutdoorTempadj+TempCadj+CO2C_logadj+SolRad_logadj|Dwelling),
                          data = datClosed,family = binomial)
}); glmmTMBm7v14$time <- timing
timing <- system.time({
  glmmTMBm7v15 <- glmmTMB(WinOpenAction ~ Group + sin((2*omega)*Hour) + sin((1*omega)*Hour) + (1+OutdoorTempadj+TempCadj+CO2C_logadj+SolRad_logadj|Dwelling),
                          data = datClosed,family = binomial)
}); glmmTMBm7v15$time <- timing

#######################
#BIC(glmmTMBm7r1)
#BIC(glmmTMBm7r2)
BIC(glmmTMBm7r3)
BIC(glmmTMBm7r4)
#BIC(glmmTMBm7r5)
BIC(glmmTMBm7r6)
BIC(glmmTMBm7r7)
#BIC(glmmTMBm7r8)
BIC(glmmTMBm7r9)

BIC(glmmTMBm7v1)
BIC(glmmTMBm7v2)
BIC(glmmTMBm7v3)
BIC(glmmTMBm7v4)
BIC(glmmTMBm7v5)
BIC(glmmTMBm7v6)
BIC(glmmTMBm7v7)
BIC(glmmTMBm7v8)
BIC(glmmTMBm7v9)
BIC(glmmTMBm7v10)
BIC(glmmTMBm7v11) #bedst
BIC(glmmTMBm7v12)
BIC(glmmTMBm7v13)
BIC(glmmTMBm7v14)
BIC(glmmTMBm7v15)

#######################################################
####################### MODEL 8 #######################
#######################################################

# timing <- system.time({
#   glmmTMBm8r1 <- glmmTMB(WinOpenAction ~ 1 + sin((3*omega)*Hour) + sin((2*omega)*Hour) + sin((1*omega)*Hour) + (1+OutdoorTempadj+TempCadj+CO2C_logadj+SolRad_logadj|Dwelling),
#                          data = datClosed,family = binomial)
# }); glmmTMBm8r1$time <- timing
# timing <- system.time({
#   glmmTMBm8r2 <- glmmTMB(WinOpenAction ~ 1 + sin((3*omega)*Hour) + sin((2*omega)*Hour) + sin((1*omega)*Hour) + (1+OutdoorTempadj+TempCadj+CO2C_logadj+SolRad_logadj|Dwelling),
#                          data = datClosed,family = binomial)
# }); glmmTMBm8r2$time <- timing
timing <- system.time({
  glmmTMBm8r3 <- glmmTMB(WinOpenAction ~ 1 + sin((3*omega)*Hour) + sin((2*omega)*Hour) + sin((1*omega)*Hour) + (1+RHCadj+OutdoorTempadj+TempCadj+CO2C_logadj+SolRad_logadj|Dwelling),
                         data = datClosed,family = binomial)
}); glmmTMBm8r3$time <- timing
timing <- system.time({
  glmmTMBm8r4 <- glmmTMB(WinOpenAction ~ 1 + sin((3*omega)*Hour) + sin((2*omega)*Hour) + sin((1*omega)*Hour) + (1+LuxC_logadj+OutdoorTempadj+TempCadj+CO2C_logadj+SolRad_logadj|Dwelling),
                         data = datClosed,family = binomial)
}); glmmTMBm8r4$time <- timing
# timing <- system.time({
#   glmmTMBm8r5 <- glmmTMB(WinOpenAction ~ 1 + sin((3*omega)*Hour) + sin((2*omega)*Hour) + sin((1*omega)*Hour) + (1+OutdoorTempadj+TempCadj+CO2C_logadj+SolRad_logadj|Dwelling),
#                          data = datClosed,family = binomial)
# }); glmmTMBm8r5$time <- timing
timing <- system.time({
  glmmTMBm8r6 <- glmmTMB(WinOpenAction ~ 1 + sin((3*omega)*Hour) + sin((2*omega)*Hour) + sin((1*omega)*Hour) + (1+Wind_logadj+OutdoorTempadj+TempCadj+CO2C_logadj+SolRad_logadj|Dwelling),
                         data = datClosed,family = binomial)
}); glmmTMBm8r6$time <- timing
timing <- system.time({
  glmmTMBm8r7 <- glmmTMB(WinOpenAction ~ 1 + sin((3*omega)*Hour) + sin((2*omega)*Hour) + sin((1*omega)*Hour) + (1+OutdoorRHadj+OutdoorTempadj+TempCadj+CO2C_logadj+SolRad_logadj|Dwelling),
                         data = datClosed,family = binomial)
}); glmmTMBm8r7$time <- timing
# timing <- system.time({
#   glmmTMBm8r8 <- glmmTMB(WinOpenAction ~ 1 + sin((3*omega)*Hour) + sin((2*omega)*Hour) + sin((1*omega)*Hour) + (1+OutdoorTempadj+TempCadj+CO2C_logadj+SolRad_logadj|Dwelling),
#                          data = datClosed,family = binomial)
# }); glmmTMBm8r8$time <- timing
timing <- system.time({
  glmmTMBm8r9 <- glmmTMB(WinOpenAction ~ 1 + sin((3*omega)*Hour) + sin((2*omega)*Hour) + sin((1*omega)*Hour) + (1+SolTimeradj+OutdoorTempadj+TempCadj+CO2C_logadj+SolRad_logadj|Dwelling),
                         data = datClosed,family = binomial)
}); glmmTMBm8r9$time <- timing

#######################
timing <- system.time({
  glmmTMBm8v1 <- glmmTMB(WinOpenAction ~ TempC + sin((3*omega)*Hour) + sin((2*omega)*Hour) + sin((1*omega)*Hour) + (1+OutdoorTempadj+TempCadj+CO2C_logadj+SolRad_logadj|Dwelling),
                         data = datClosed,family = binomial)
}); glmmTMBm8v1$time <- timing
timing <- system.time({
  glmmTMBm8v2 <- glmmTMB(WinOpenAction ~ CO2C_log + sin((3*omega)*Hour) + sin((2*omega)*Hour) + sin((1*omega)*Hour) + (1+OutdoorTempadj+TempCadj+CO2C_logadj+SolRad_logadj|Dwelling),
                         data = datClosed,family = binomial)
}); glmmTMBm8v2$time <- timing
timing <- system.time({
  glmmTMBm8v3 <- glmmTMB(WinOpenAction ~ RHC + sin((3*omega)*Hour) + sin((2*omega)*Hour) + sin((1*omega)*Hour) + (1+OutdoorTempadj+TempCadj+CO2C_logadj+SolRad_logadj|Dwelling),
                         data = datClosed,family = binomial)
}); glmmTMBm8v3$time <- timing
timing <- system.time({
  glmmTMBm8v4 <- glmmTMB(WinOpenAction ~ LuxC_log + sin((3*omega)*Hour) + sin((2*omega)*Hour) + sin((1*omega)*Hour) + (1+OutdoorTempadj+TempCadj+CO2C_logadj+SolRad_logadj|Dwelling),
                         data = datClosed,family = binomial)
}); glmmTMBm8v4$time <- timing
timing <- system.time({
  glmmTMBm8v5 <- glmmTMB(WinOpenAction ~ OutdoorTemp + sin((3*omega)*Hour) + sin((2*omega)*Hour) + sin((1*omega)*Hour) + (1+OutdoorTempadj+TempCadj+CO2C_logadj+SolRad_logadj|Dwelling),
                         data = datClosed,family = binomial)
}); glmmTMBm8v5$time <- timing
timing <- system.time({
  glmmTMBm8v6 <- glmmTMB(WinOpenAction ~ Wind_log + sin((3*omega)*Hour) + sin((2*omega)*Hour) + sin((1*omega)*Hour) + (1+OutdoorTempadj+TempCadj+CO2C_logadj+SolRad_logadj|Dwelling),
                         data = datClosed,family = binomial)
}); glmmTMBm8v6$time <- timing
timing <- system.time({
  glmmTMBm8v7 <- glmmTMB(WinOpenAction ~ OutdoorRH + sin((3*omega)*Hour) + sin((2*omega)*Hour) + sin((1*omega)*Hour) + (1+OutdoorTempadj+TempCadj+CO2C_logadj+SolRad_logadj|Dwelling),
                         data = datClosed,family = binomial)
}); glmmTMBm8v7$time <- timing
timing <- system.time({
  glmmTMBm8v8 <- glmmTMB(WinOpenAction ~ SolRad_log + sin((3*omega)*Hour) + sin((2*omega)*Hour) + sin((1*omega)*Hour) + (1+OutdoorTempadj+TempCadj+CO2C_logadj+SolRad_logadj|Dwelling),
                         data = datClosed,family = binomial)
}); glmmTMBm8v8$time <- timing
timing <- system.time({
  glmmTMBm8v9 <- glmmTMB(WinOpenAction ~ SolTimer + sin((3*omega)*Hour) + sin((2*omega)*Hour) + sin((1*omega)*Hour) + (1+OutdoorTempadj+TempCadj+CO2C_logadj+SolRad_logadj|Dwelling),
                         data = datClosed,family = binomial)
}); glmmTMBm8v9$time <- timing
timing <- system.time({
  glmmTMBm8v10 <- glmmTMB(WinOpenAction ~ cos((1*omega)*Hour) + sin((3*omega)*Hour) + sin((2*omega)*Hour) + sin((1*omega)*Hour) + (1+OutdoorTempadj+TempCadj+CO2C_logadj+SolRad_logadj|Dwelling),
                          data = datClosed,family = binomial)
}); glmmTMBm8v10$time <- timing
timing <- system.time({
  glmmTMBm8v11 <- glmmTMB(WinOpenAction ~ sin((4*omega)*Hour) + sin((3*omega)*Hour) + sin((2*omega)*Hour) + sin((1*omega)*Hour) + (1+OutdoorTempadj+TempCadj+CO2C_logadj+SolRad_logadj|Dwelling),
                          data = datClosed,family = binomial)
}); glmmTMBm8v11$time <- timing
timing <- system.time({
  glmmTMBm8v12 <- glmmTMB(WinOpenAction ~ Room + sin((3*omega)*Hour) + sin((2*omega)*Hour) + sin((1*omega)*Hour) + (1+OutdoorTempadj+TempCadj+CO2C_logadj+SolRad_logadj|Dwelling),
                          data = datClosed,family = binomial)
}); glmmTMBm8v12$time <- timing
timing <- system.time({
  glmmTMBm8v13 <- glmmTMB(WinOpenAction ~ Weekday + sin((3*omega)*Hour) + sin((2*omega)*Hour) + sin((1*omega)*Hour) + (1+OutdoorTempadj+TempCadj+CO2C_logadj+SolRad_logadj|Dwelling),
                          data = datClosed,family = binomial)
}); glmmTMBm8v13$time <- timing
timing <- system.time({
  glmmTMBm8v14 <- glmmTMB(WinOpenAction ~ Season + sin((3*omega)*Hour) + sin((2*omega)*Hour) + sin((1*omega)*Hour) + (1+OutdoorTempadj+TempCadj+CO2C_logadj+SolRad_logadj|Dwelling),
                          data = datClosed,family = binomial)
}); glmmTMBm8v14$time <- timing
timing <- system.time({
  glmmTMBm8v15 <- glmmTMB(WinOpenAction ~ Group + sin((3*omega)*Hour) + sin((2*omega)*Hour) + sin((1*omega)*Hour) + (1+OutdoorTempadj+TempCadj+CO2C_logadj+SolRad_logadj|Dwelling),
                          data = datClosed,family = binomial)
}); glmmTMBm8v15$time <- timing

#######################
#BIC(glmmTMBm8r1)
#BIC(glmmTMBm8r2)
BIC(glmmTMBm8r3)
BIC(glmmTMBm8r4)
#BIC(glmmTMBm8r5)
BIC(glmmTMBm8r6)
BIC(glmmTMBm8r7)
#BIC(glmmTMBm8r8)
BIC(glmmTMBm8r9)

BIC(glmmTMBm8v1)
BIC(glmmTMBm8v2)
BIC(glmmTMBm8v3)
BIC(glmmTMBm8v4)
BIC(glmmTMBm8v5)
BIC(glmmTMBm8v6)
BIC(glmmTMBm8v7)
BIC(glmmTMBm8v8)
BIC(glmmTMBm8v9)
BIC(glmmTMBm8v10)
BIC(glmmTMBm8v11) #bedst
BIC(glmmTMBm8v12)
BIC(glmmTMBm8v13)
BIC(glmmTMBm8v14)
BIC(glmmTMBm8v15)

#######################################################
####################### MODEL 9 #######################
#######################################################

# timing <- system.time({
#   glmmTMBm9r1 <- glmmTMB(WinOpenAction ~ 1 + sin((4*omega)*Hour) + sin((3*omega)*Hour) + sin((2*omega)*Hour) + sin((1*omega)*Hour) + (1+OutdoorTempadj+TempCadj+CO2C_logadj+SolRad_logadj|Dwelling),
#                          data = datClosed,family = binomial)
# }); glmmTMBm9r1$time <- timing
# timing <- system.time({
#   glmmTMBm9r2 <- glmmTMB(WinOpenAction ~ 1 + sin((4*omega)*Hour) + sin((3*omega)*Hour) + sin((2*omega)*Hour) + sin((1*omega)*Hour) + (1+OutdoorTempadj+TempCadj+CO2C_logadj+SolRad_logadj|Dwelling),
#                          data = datClosed,family = binomial)
# }); glmmTMBm9r2$time <- timing
timing <- system.time({
  glmmTMBm9r3 <- glmmTMB(WinOpenAction ~ 1 + sin((4*omega)*Hour) + sin((3*omega)*Hour) + sin((2*omega)*Hour) + sin((1*omega)*Hour) + (1+RHCadj+OutdoorTempadj+TempCadj+CO2C_logadj+SolRad_logadj|Dwelling),
                         data = datClosed,family = binomial)
}); glmmTMBm9r3$time <- timing
timing <- system.time({
  glmmTMBm9r4 <- glmmTMB(WinOpenAction ~ 1 + sin((4*omega)*Hour) + sin((3*omega)*Hour) + sin((2*omega)*Hour) + sin((1*omega)*Hour) + (1+LuxC_logadj+OutdoorTempadj+TempCadj+CO2C_logadj+SolRad_logadj|Dwelling),
                         data = datClosed,family = binomial)
}); glmmTMBm9r4$time <- timing
# timing <- system.time({
#   glmmTMBm9r5 <- glmmTMB(WinOpenAction ~ 1 + sin((4*omega)*Hour) + sin((3*omega)*Hour) + sin((2*omega)*Hour) + sin((1*omega)*Hour) + (1+OutdoorTempadj+TempCadj+CO2C_logadj+SolRad_logadj|Dwelling),
#                          data = datClosed,family = binomial)
# }); glmmTMBm9r5$time <- timing
timing <- system.time({
  glmmTMBm9r6 <- glmmTMB(WinOpenAction ~ 1 + sin((4*omega)*Hour) + sin((3*omega)*Hour) + sin((2*omega)*Hour) + sin((1*omega)*Hour) + (1+Wind_logadj+OutdoorTempadj+TempCadj+CO2C_logadj+SolRad_logadj|Dwelling),
                         data = datClosed,family = binomial)
}); glmmTMBm9r6$time <- timing
timing <- system.time({
  glmmTMBm9r7 <- glmmTMB(WinOpenAction ~ 1 + sin((4*omega)*Hour) + sin((3*omega)*Hour) + sin((2*omega)*Hour) + sin((1*omega)*Hour) + (1+OutdoorRHadj+OutdoorTempadj+TempCadj+CO2C_logadj+SolRad_logadj|Dwelling),
                         data = datClosed,family = binomial)
}); glmmTMBm9r7$time <- timing
# timing <- system.time({
#   glmmTMBm9r8 <- glmmTMB(WinOpenAction ~ 1 + sin((4*omega)*Hour) + sin((3*omega)*Hour) + sin((2*omega)*Hour) + sin((1*omega)*Hour) + (1+OutdoorTempadj+TempCadj+CO2C_logadj+SolRad_logadj|Dwelling),
#                          data = datClosed,family = binomial)
# }); glmmTMBm9r8$time <- timing
timing <- system.time({
  glmmTMBm9r9 <- glmmTMB(WinOpenAction ~ 1 + sin((4*omega)*Hour) + sin((3*omega)*Hour) + sin((2*omega)*Hour) + sin((1*omega)*Hour) + (1+SolTimeradj+OutdoorTempadj+TempCadj+CO2C_logadj+SolRad_logadj|Dwelling),
                         data = datClosed,family = binomial)
}); glmmTMBm9r9$time <- timing

#######################
timing <- system.time({
  glmmTMBm9v1 <- glmmTMB(WinOpenAction ~ TempC + sin((4*omega)*Hour) + sin((3*omega)*Hour) + sin((2*omega)*Hour) + sin((1*omega)*Hour) + (1+OutdoorTempadj+TempCadj+CO2C_logadj+SolRad_logadj|Dwelling),
                         data = datClosed,family = binomial)
}); glmmTMBm9v1$time <- timing
timing <- system.time({
  glmmTMBm9v2 <- glmmTMB(WinOpenAction ~ CO2C_log + sin((4*omega)*Hour) + sin((3*omega)*Hour) + sin((2*omega)*Hour) + sin((1*omega)*Hour) + (1+OutdoorTempadj+TempCadj+CO2C_logadj+SolRad_logadj|Dwelling),
                         data = datClosed,family = binomial)
}); glmmTMBm9v2$time <- timing
timing <- system.time({
  glmmTMBm9v3 <- glmmTMB(WinOpenAction ~ RHC + sin((4*omega)*Hour) + sin((3*omega)*Hour) + sin((2*omega)*Hour) + sin((1*omega)*Hour) + (1+OutdoorTempadj+TempCadj+CO2C_logadj+SolRad_logadj|Dwelling),
                         data = datClosed,family = binomial)
}); glmmTMBm9v3$time <- timing
timing <- system.time({
  glmmTMBm9v4 <- glmmTMB(WinOpenAction ~ LuxC_log + sin((4*omega)*Hour) + sin((3*omega)*Hour) + sin((2*omega)*Hour) + sin((1*omega)*Hour) + (1+OutdoorTempadj+TempCadj+CO2C_logadj+SolRad_logadj|Dwelling),
                         data = datClosed,family = binomial)
}); glmmTMBm9v4$time <- timing
timing <- system.time({
  glmmTMBm9v5 <- glmmTMB(WinOpenAction ~ OutdoorTemp + sin((4*omega)*Hour) + sin((3*omega)*Hour) + sin((2*omega)*Hour) + sin((1*omega)*Hour) + (1+OutdoorTempadj+TempCadj+CO2C_logadj+SolRad_logadj|Dwelling),
                         data = datClosed,family = binomial)
}); glmmTMBm9v5$time <- timing
timing <- system.time({
  glmmTMBm9v6 <- glmmTMB(WinOpenAction ~ Wind_log + sin((4*omega)*Hour) + sin((3*omega)*Hour) + sin((2*omega)*Hour) + sin((1*omega)*Hour) + (1+OutdoorTempadj+TempCadj+CO2C_logadj+SolRad_logadj|Dwelling),
                         data = datClosed,family = binomial)
}); glmmTMBm9v6$time <- timing
timing <- system.time({
  glmmTMBm9v7 <- glmmTMB(WinOpenAction ~ OutdoorRH + sin((4*omega)*Hour) + sin((3*omega)*Hour) + sin((2*omega)*Hour) + sin((1*omega)*Hour) + (1+OutdoorTempadj+TempCadj+CO2C_logadj+SolRad_logadj|Dwelling),
                         data = datClosed,family = binomial)
}); glmmTMBm9v7$time <- timing
timing <- system.time({
  glmmTMBm9v8 <- glmmTMB(WinOpenAction ~ SolRad_log + sin((4*omega)*Hour) + sin((3*omega)*Hour) + sin((2*omega)*Hour) + sin((1*omega)*Hour) + (1+OutdoorTempadj+TempCadj+CO2C_logadj+SolRad_logadj|Dwelling),
                         data = datClosed,family = binomial)
}); glmmTMBm9v8$time <- timing
timing <- system.time({
  glmmTMBm9v9 <- glmmTMB(WinOpenAction ~ SolTimer + sin((4*omega)*Hour) + sin((3*omega)*Hour) + sin((2*omega)*Hour) + sin((1*omega)*Hour) + (1+OutdoorTempadj+TempCadj+CO2C_logadj+SolRad_logadj|Dwelling),
                         data = datClosed,family = binomial)
}); glmmTMBm9v9$time <- timing
timing <- system.time({
  glmmTMBm9v10 <- glmmTMB(WinOpenAction ~ cos((1*omega)*Hour) + sin((4*omega)*Hour) + sin((3*omega)*Hour) + sin((2*omega)*Hour) + sin((1*omega)*Hour) + (1+OutdoorTempadj+TempCadj+CO2C_logadj+SolRad_logadj|Dwelling),
                          data = datClosed,family = binomial)
}); glmmTMBm9v10$time <- timing
# timing <- system.time({
#   glmmTMBm9v11 <- glmmTMB(WinOpenAction ~ sin((4*omega)*Hour) + sin((3*omega)*Hour) + sin((2*omega)*Hour) + sin((1*omega)*Hour) + (1+OutdoorTempadj+TempCadj+CO2C_logadj+SolRad_logadj|Dwelling),
#                           data = datClosed,family = binomial)
# }); glmmTMBm9v11$time <- timing
timing <- system.time({
  glmmTMBm9v12 <- glmmTMB(WinOpenAction ~ Room + sin((4*omega)*Hour) + sin((3*omega)*Hour) + sin((2*omega)*Hour) + sin((1*omega)*Hour) + (1+OutdoorTempadj+TempCadj+CO2C_logadj+SolRad_logadj|Dwelling),
                          data = datClosed,family = binomial)
}); glmmTMBm9v12$time <- timing
timing <- system.time({
  glmmTMBm9v13 <- glmmTMB(WinOpenAction ~ Weekday + sin((4*omega)*Hour) + sin((3*omega)*Hour) + sin((2*omega)*Hour) + sin((1*omega)*Hour) + (1+OutdoorTempadj+TempCadj+CO2C_logadj+SolRad_logadj|Dwelling),
                          data = datClosed,family = binomial)
}); glmmTMBm9v13$time <- timing
timing <- system.time({
  glmmTMBm9v14 <- glmmTMB(WinOpenAction ~ Season + sin((4*omega)*Hour) + sin((3*omega)*Hour) + sin((2*omega)*Hour) + sin((1*omega)*Hour) + (1+OutdoorTempadj+TempCadj+CO2C_logadj+SolRad_logadj|Dwelling),
                          data = datClosed,family = binomial)
}); glmmTMBm9v14$time <- timing
timing <- system.time({
  glmmTMBm9v15 <- glmmTMB(WinOpenAction ~ Group + sin((4*omega)*Hour) + sin((3*omega)*Hour) + sin((2*omega)*Hour) + sin((1*omega)*Hour) + (1+OutdoorTempadj+TempCadj+CO2C_logadj+SolRad_logadj|Dwelling),
                          data = datClosed,family = binomial)
}); glmmTMBm9v15$time <- timing

#######################
#BIC(glmmTMBm9r1)
#BIC(glmmTMBm9r2)
BIC(glmmTMBm9r3)
BIC(glmmTMBm9r4)
#BIC(glmmTMBm9r5)
BIC(glmmTMBm9r6)
BIC(glmmTMBm9r7)
#BIC(glmmTMBm9r8)
BIC(glmmTMBm9r9)

BIC(glmmTMBm9v1)
BIC(glmmTMBm9v2)
BIC(glmmTMBm9v3)
BIC(glmmTMBm9v4)
BIC(glmmTMBm9v5)
BIC(glmmTMBm9v6)
BIC(glmmTMBm9v7)
BIC(glmmTMBm9v8)
BIC(glmmTMBm9v9)
BIC(glmmTMBm9v10)
#BIC(glmmTMBm9v11)
BIC(glmmTMBm9v12) #bedst
BIC(glmmTMBm9v13)
BIC(glmmTMBm9v14)
BIC(glmmTMBm9v15)

#######################################################
####################### MODEL 10 ######################
#######################################################

# timing <- system.time({
#   glmmTMBm10r1 <- glmmTMB(WinOpenAction ~ 1 + Room + sin((4*omega)*Hour) + sin((3*omega)*Hour) + sin((2*omega)*Hour) + sin((1*omega)*Hour) + (1+OutdoorTempadj+TempCadj+CO2C_logadj+SolRad_logadj|Dwelling),
#                          data = datClosed,family = binomial)
# }); glmmTMBm10r1$time <- timing
# timing <- system.time({
#   glmmTMBm10r2 <- glmmTMB(WinOpenAction ~ 1 + Room + sin((4*omega)*Hour) + sin((3*omega)*Hour) + sin((2*omega)*Hour) + sin((1*omega)*Hour) + (1+OutdoorTempadj+TempCadj+CO2C_logadj+SolRad_logadj|Dwelling),
#                          data = datClosed,family = binomial)
# }); glmmTMBm10r2$time <- timing
timing <- system.time({
  glmmTMBm10r3 <- glmmTMB(WinOpenAction ~ 1 + Room + sin((4*omega)*Hour) + sin((3*omega)*Hour) + sin((2*omega)*Hour) + sin((1*omega)*Hour) + (1+RHCadj+OutdoorTempadj+TempCadj+CO2C_logadj+SolRad_logadj|Dwelling),
                          data = datClosed,family = binomial)
}); glmmTMBm10r3$time <- timing
timing <- system.time({
  glmmTMBm10r4 <- glmmTMB(WinOpenAction ~ 1 + Room + sin((4*omega)*Hour) + sin((3*omega)*Hour) + sin((2*omega)*Hour) + sin((1*omega)*Hour) + (1+LuxC_logadj+OutdoorTempadj+TempCadj+CO2C_logadj+SolRad_logadj|Dwelling),
                          data = datClosed,family = binomial)
}); glmmTMBm10r4$time <- timing
# timing <- system.time({
#   glmmTMBm10r5 <- glmmTMB(WinOpenAction ~ 1 + Room + sin((4*omega)*Hour) + sin((3*omega)*Hour) + sin((2*omega)*Hour) + sin((1*omega)*Hour) + (1+OutdoorTempadj+TempCadj+CO2C_logadj+SolRad_logadj|Dwelling),
#                          data = datClosed,family = binomial)
# }); glmmTMBm10r5$time <- timing
timing <- system.time({
  glmmTMBm10r6 <- glmmTMB(WinOpenAction ~ 1 + Room + sin((4*omega)*Hour) + sin((3*omega)*Hour) + sin((2*omega)*Hour) + sin((1*omega)*Hour) + (1+Wind_logadj+OutdoorTempadj+TempCadj+CO2C_logadj+SolRad_logadj|Dwelling),
                          data = datClosed,family = binomial)
}); glmmTMBm10r6$time <- timing
timing <- system.time({
  glmmTMBm10r7 <- glmmTMB(WinOpenAction ~ 1 + Room + sin((4*omega)*Hour) + sin((3*omega)*Hour) + sin((2*omega)*Hour) + sin((1*omega)*Hour) + (1+OutdoorRHadj+OutdoorTempadj+TempCadj+CO2C_logadj+SolRad_logadj|Dwelling),
                          data = datClosed,family = binomial)
}); glmmTMBm10r7$time <- timing
# timing <- system.time({
#   glmmTMBm10r8 <- glmmTMB(WinOpenAction ~ 1 + Room + sin((4*omega)*Hour) + sin((3*omega)*Hour) + sin((2*omega)*Hour) + sin((1*omega)*Hour) + (1+OutdoorTempadj+TempCadj+CO2C_logadj+SolRad_logadj|Dwelling),
#                          data = datClosed,family = binomial)
# }); glmmTMBm10r8$time <- timing
timing <- system.time({
  glmmTMBm10r9 <- glmmTMB(WinOpenAction ~ 1 + Room + sin((4*omega)*Hour) + sin((3*omega)*Hour) + sin((2*omega)*Hour) + sin((1*omega)*Hour) + (1+SolTimeradj+OutdoorTempadj+TempCadj+CO2C_logadj+SolRad_logadj|Dwelling),
                          data = datClosed,family = binomial)
}); glmmTMBm10r9$time <- timing

#######################
timing <- system.time({
  glmmTMBm10v1 <- glmmTMB(WinOpenAction ~ TempC + Room + sin((4*omega)*Hour) + sin((3*omega)*Hour) + sin((2*omega)*Hour) + sin((1*omega)*Hour) + (1+OutdoorTempadj+TempCadj+CO2C_logadj+SolRad_logadj|Dwelling),
                          data = datClosed,family = binomial)
}); glmmTMBm10v1$time <- timing
timing <- system.time({
  glmmTMBm10v2 <- glmmTMB(WinOpenAction ~ CO2C_log + Room + sin((4*omega)*Hour) + sin((3*omega)*Hour) + sin((2*omega)*Hour) + sin((1*omega)*Hour) + (1+OutdoorTempadj+TempCadj+CO2C_logadj+SolRad_logadj|Dwelling),
                          data = datClosed,family = binomial)
}); glmmTMBm10v2$time <- timing
timing <- system.time({
  glmmTMBm10v3 <- glmmTMB(WinOpenAction ~ RHC + Room + sin((4*omega)*Hour) + sin((3*omega)*Hour) + sin((2*omega)*Hour) + sin((1*omega)*Hour) + (1+OutdoorTempadj+TempCadj+CO2C_logadj+SolRad_logadj|Dwelling),
                          data = datClosed,family = binomial)
}); glmmTMBm10v3$time <- timing
timing <- system.time({
  glmmTMBm10v4 <- glmmTMB(WinOpenAction ~ LuxC_log + Room + sin((4*omega)*Hour) + sin((3*omega)*Hour) + sin((2*omega)*Hour) + sin((1*omega)*Hour) + (1+OutdoorTempadj+TempCadj+CO2C_logadj+SolRad_logadj|Dwelling),
                          data = datClosed,family = binomial)
}); glmmTMBm10v4$time <- timing
timing <- system.time({
  glmmTMBm10v5 <- glmmTMB(WinOpenAction ~ OutdoorTemp + Room + sin((4*omega)*Hour) + sin((3*omega)*Hour) + sin((2*omega)*Hour) + sin((1*omega)*Hour) + (1+OutdoorTempadj+TempCadj+CO2C_logadj+SolRad_logadj|Dwelling),
                          data = datClosed,family = binomial)
}); glmmTMBm10v5$time <- timing
timing <- system.time({
  glmmTMBm10v6 <- glmmTMB(WinOpenAction ~ Wind_log + Room + sin((4*omega)*Hour) + sin((3*omega)*Hour) + sin((2*omega)*Hour) + sin((1*omega)*Hour) + (1+OutdoorTempadj+TempCadj+CO2C_logadj+SolRad_logadj|Dwelling),
                          data = datClosed,family = binomial)
}); glmmTMBm10v6$time <- timing
timing <- system.time({
  glmmTMBm10v7 <- glmmTMB(WinOpenAction ~ OutdoorRH + Room + sin((4*omega)*Hour) + sin((3*omega)*Hour) + sin((2*omega)*Hour) + sin((1*omega)*Hour) + (1+OutdoorTempadj+TempCadj+CO2C_logadj+SolRad_logadj|Dwelling),
                          data = datClosed,family = binomial)
}); glmmTMBm10v7$time <- timing
timing <- system.time({
  glmmTMBm10v8 <- glmmTMB(WinOpenAction ~ SolRad_log + Room + sin((4*omega)*Hour) + sin((3*omega)*Hour) + sin((2*omega)*Hour) + sin((1*omega)*Hour) + (1+OutdoorTempadj+TempCadj+CO2C_logadj+SolRad_logadj|Dwelling),
                          data = datClosed,family = binomial)
}); glmmTMBm10v8$time <- timing
timing <- system.time({
  glmmTMBm10v9 <- glmmTMB(WinOpenAction ~ SolTimer + Room + sin((4*omega)*Hour) + sin((3*omega)*Hour) + sin((2*omega)*Hour) + sin((1*omega)*Hour) + (1+OutdoorTempadj+TempCadj+CO2C_logadj+SolRad_logadj|Dwelling),
                          data = datClosed,family = binomial)
}); glmmTMBm10v9$time <- timing
timing <- system.time({
  glmmTMBm10v10 <- glmmTMB(WinOpenAction ~ cos((1*omega)*Hour) + Room + sin((4*omega)*Hour) + sin((3*omega)*Hour) + sin((2*omega)*Hour) + sin((1*omega)*Hour) + (1+OutdoorTempadj+TempCadj+CO2C_logadj+SolRad_logadj|Dwelling),
                           data = datClosed,family = binomial)
}); glmmTMBm10v10$time <- timing
# timing <- system.time({
#   glmmTMBm10v11 <- glmmTMB(WinOpenAction ~ Room + sin((4*omega)*Hour) + sin((3*omega)*Hour) + sin((2*omega)*Hour) + sin((1*omega)*Hour) + (1+OutdoorTempadj+TempCadj+CO2C_logadj+SolRad_logadj|Dwelling),
#                           data = datClosed,family = binomial)
# }); glmmTMBm10v11$time <- timing
# timing <- system.time({
#   glmmTMBm10v12 <- glmmTMB(WinOpenAction ~ Room + sin((4*omega)*Hour) + sin((3*omega)*Hour) + sin((2*omega)*Hour) + sin((1*omega)*Hour) + (1+OutdoorTempadj+TempCadj+CO2C_logadj+SolRad_logadj|Dwelling),
#                           data = datClosed,family = binomial)
# }); glmmTMBm10v12$time <- timing
timing <- system.time({
  glmmTMBm10v13 <- glmmTMB(WinOpenAction ~ Weekday + Room + sin((4*omega)*Hour) + sin((3*omega)*Hour) + sin((2*omega)*Hour) + sin((1*omega)*Hour) + (1+OutdoorTempadj+TempCadj+CO2C_logadj+SolRad_logadj|Dwelling),
                           data = datClosed,family = binomial)
}); glmmTMBm10v13$time <- timing
timing <- system.time({
  glmmTMBm10v14 <- glmmTMB(WinOpenAction ~ Season + Room + sin((4*omega)*Hour) + sin((3*omega)*Hour) + sin((2*omega)*Hour) + sin((1*omega)*Hour) + (1+OutdoorTempadj+TempCadj+CO2C_logadj+SolRad_logadj|Dwelling),
                           data = datClosed,family = binomial)
}); glmmTMBm10v14$time <- timing
timing <- system.time({
  glmmTMBm10v15 <- glmmTMB(WinOpenAction ~ Group + Room + sin((4*omega)*Hour) + sin((3*omega)*Hour) + sin((2*omega)*Hour) + sin((1*omega)*Hour) + (1+OutdoorTempadj+TempCadj+CO2C_logadj+SolRad_logadj|Dwelling),
                           data = datClosed,family = binomial)
}); glmmTMBm10v15$time <- timing

#######################
#BIC(glmmTMBm10r1)
#BIC(glmmTMBm10r2)
BIC(glmmTMBm10r3)
BIC(glmmTMBm10r4)
#BIC(glmmTMBm10r5)
BIC(glmmTMBm10r6)
BIC(glmmTMBm10r7)
#BIC(glmmTMBm10r8)
BIC(glmmTMBm10r9)

BIC(glmmTMBm10v1)
BIC(glmmTMBm10v2)
BIC(glmmTMBm10v3)
BIC(glmmTMBm10v4)
BIC(glmmTMBm10v5)
BIC(glmmTMBm10v6)
BIC(glmmTMBm10v7)
BIC(glmmTMBm10v8)
BIC(glmmTMBm10v9)
BIC(glmmTMBm10v10)
#BIC(glmmTMBm10v11)
#BIC(glmmTMBm10v12)
BIC(glmmTMBm10v13)
BIC(glmmTMBm10v14) #bedst
BIC(glmmTMBm10v15)

#######################################################
####################### MODEL 11 ######################
#######################################################

# timing <- system.time({
#   glmmTMBm11r1 <- glmmTMB(WinOpenAction ~ 1 + Season + Room + sin((4*omega)*Hour) + sin((3*omega)*Hour) + sin((2*omega)*Hour) + sin((1*omega)*Hour) + (1+OutdoorTempadj+TempCadj+CO2C_logadj+SolRad_logadj|Dwelling),
#                          data = datClosed,family = binomial)
# }); glmmTMBm11r1$time <- timing
# timing <- system.time({
#   glmmTMBm11r2 <- glmmTMB(WinOpenAction ~ 1 + Season + Room + sin((4*omega)*Hour) + sin((3*omega)*Hour) + sin((2*omega)*Hour) + sin((1*omega)*Hour) + (1+OutdoorTempadj+TempCadj+CO2C_logadj+SolRad_logadj|Dwelling),
#                          data = datClosed,family = binomial)
# }); glmmTMBm11r2$time <- timing
timing <- system.time({
  glmmTMBm11r3 <- glmmTMB(WinOpenAction ~ 1 + Season + Room + sin((4*omega)*Hour) + sin((3*omega)*Hour) + sin((2*omega)*Hour) + sin((1*omega)*Hour) + (1+RHCadj+OutdoorTempadj+TempCadj+CO2C_logadj+SolRad_logadj|Dwelling),
                          data = datClosed,family = binomial)
}); glmmTMBm11r3$time <- timing
timing <- system.time({
  glmmTMBm11r4 <- glmmTMB(WinOpenAction ~ 1 + Season + Room + sin((4*omega)*Hour) + sin((3*omega)*Hour) + sin((2*omega)*Hour) + sin((1*omega)*Hour) + (1+LuxC_logadj+OutdoorTempadj+TempCadj+CO2C_logadj+SolRad_logadj|Dwelling),
                          data = datClosed,family = binomial)
}); glmmTMBm11r4$time <- timing
# timing <- system.time({
#   glmmTMBm11r5 <- glmmTMB(WinOpenAction ~ 1 + Season + Room + sin((4*omega)*Hour) + sin((3*omega)*Hour) + sin((2*omega)*Hour) + sin((1*omega)*Hour) + (1+OutdoorTempadj+TempCadj+CO2C_logadj+SolRad_logadj|Dwelling),
#                          data = datClosed,family = binomial)
# }); glmmTMBm11r5$time <- timing
timing <- system.time({
  glmmTMBm11r6 <- glmmTMB(WinOpenAction ~ 1 + Season + Room + sin((4*omega)*Hour) + sin((3*omega)*Hour) + sin((2*omega)*Hour) + sin((1*omega)*Hour) + (1+Wind_logadj+OutdoorTempadj+TempCadj+CO2C_logadj+SolRad_logadj|Dwelling),
                          data = datClosed,family = binomial)
}); glmmTMBm11r6$time <- timing
timing <- system.time({
  glmmTMBm11r7 <- glmmTMB(WinOpenAction ~ 1 + Season + Room + sin((4*omega)*Hour) + sin((3*omega)*Hour) + sin((2*omega)*Hour) + sin((1*omega)*Hour) + (1+OutdoorRHadj+OutdoorTempadj+TempCadj+CO2C_logadj+SolRad_logadj|Dwelling),
                          data = datClosed,family = binomial)
}); glmmTMBm11r7$time <- timing
# timing <- system.time({
#   glmmTMBm11r8 <- glmmTMB(WinOpenAction ~ 1 + Season + Room + sin((4*omega)*Hour) + sin((3*omega)*Hour) + sin((2*omega)*Hour) + sin((1*omega)*Hour) + (1+OutdoorTempadj+TempCadj+CO2C_logadj+SolRad_logadj|Dwelling),
#                          data = datClosed,family = binomial)
# }); glmmTMBm11r8$time <- timing
timing <- system.time({
  glmmTMBm11r9 <- glmmTMB(WinOpenAction ~ 1 + Season + Room + sin((4*omega)*Hour) + sin((3*omega)*Hour) + sin((2*omega)*Hour) + sin((1*omega)*Hour) + (1+SolTimeradj+OutdoorTempadj+TempCadj+CO2C_logadj+SolRad_logadj|Dwelling),
                          data = datClosed,family = binomial)
}); glmmTMBm11r9$time <- timing

#######################
timing <- system.time({
  glmmTMBm11v1 <- glmmTMB(WinOpenAction ~ TempC + Season + Room + sin((4*omega)*Hour) + sin((3*omega)*Hour) + sin((2*omega)*Hour) + sin((1*omega)*Hour) + (1+OutdoorTempadj+TempCadj+CO2C_logadj+SolRad_logadj|Dwelling),
                          data = datClosed,family = binomial)
}); glmmTMBm11v1$time <- timing
timing <- system.time({
  glmmTMBm11v2 <- glmmTMB(WinOpenAction ~ CO2C_log + Season + Room + sin((4*omega)*Hour) + sin((3*omega)*Hour) + sin((2*omega)*Hour) + sin((1*omega)*Hour) + (1+OutdoorTempadj+TempCadj+CO2C_logadj+SolRad_logadj|Dwelling),
                          data = datClosed,family = binomial)
}); glmmTMBm11v2$time <- timing
timing <- system.time({
  glmmTMBm11v3 <- glmmTMB(WinOpenAction ~ RHC + Season + Room + sin((4*omega)*Hour) + sin((3*omega)*Hour) + sin((2*omega)*Hour) + sin((1*omega)*Hour) + (1+OutdoorTempadj+TempCadj+CO2C_logadj+SolRad_logadj|Dwelling),
                          data = datClosed,family = binomial)
}); glmmTMBm11v3$time <- timing
timing <- system.time({
  glmmTMBm11v4 <- glmmTMB(WinOpenAction ~ LuxC_log + Season + Room + sin((4*omega)*Hour) + sin((3*omega)*Hour) + sin((2*omega)*Hour) + sin((1*omega)*Hour) + (1+OutdoorTempadj+TempCadj+CO2C_logadj+SolRad_logadj|Dwelling),
                          data = datClosed,family = binomial)
}); glmmTMBm11v4$time <- timing
timing <- system.time({
  glmmTMBm11v5 <- glmmTMB(WinOpenAction ~ OutdoorTemp + Season + Room + sin((4*omega)*Hour) + sin((3*omega)*Hour) + sin((2*omega)*Hour) + sin((1*omega)*Hour) + (1+OutdoorTempadj+TempCadj+CO2C_logadj+SolRad_logadj|Dwelling),
                          data = datClosed,family = binomial)
}); glmmTMBm11v5$time <- timing
timing <- system.time({
  glmmTMBm11v6 <- glmmTMB(WinOpenAction ~ Wind_log + Season + Room + sin((4*omega)*Hour) + sin((3*omega)*Hour) + sin((2*omega)*Hour) + sin((1*omega)*Hour) + (1+OutdoorTempadj+TempCadj+CO2C_logadj+SolRad_logadj|Dwelling),
                          data = datClosed,family = binomial)
}); glmmTMBm11v6$time <- timing
timing <- system.time({
  glmmTMBm11v7 <- glmmTMB(WinOpenAction ~ OutdoorRH + Season + Room + sin((4*omega)*Hour) + sin((3*omega)*Hour) + sin((2*omega)*Hour) + sin((1*omega)*Hour) + (1+OutdoorTempadj+TempCadj+CO2C_logadj+SolRad_logadj|Dwelling),
                          data = datClosed,family = binomial)
}); glmmTMBm11v7$time <- timing
timing <- system.time({
  glmmTMBm11v8 <- glmmTMB(WinOpenAction ~ SolRad_log + Season + Room + sin((4*omega)*Hour) + sin((3*omega)*Hour) + sin((2*omega)*Hour) + sin((1*omega)*Hour) + (1+OutdoorTempadj+TempCadj+CO2C_logadj+SolRad_logadj|Dwelling),
                          data = datClosed,family = binomial)
}); glmmTMBm11v8$time <- timing
timing <- system.time({
  glmmTMBm11v9 <- glmmTMB(WinOpenAction ~ SolTimer + Season + Room + sin((4*omega)*Hour) + sin((3*omega)*Hour) + sin((2*omega)*Hour) + sin((1*omega)*Hour) + (1+OutdoorTempadj+TempCadj+CO2C_logadj+SolRad_logadj|Dwelling),
                          data = datClosed,family = binomial)
}); glmmTMBm11v9$time <- timing
timing <- system.time({
  glmmTMBm11v10 <- glmmTMB(WinOpenAction ~ cos((1*omega)*Hour) + Season + Room + sin((4*omega)*Hour) + sin((3*omega)*Hour) + sin((2*omega)*Hour) + sin((1*omega)*Hour) + (1+OutdoorTempadj+TempCadj+CO2C_logadj+SolRad_logadj|Dwelling),
                           data = datClosed,family = binomial)
}); glmmTMBm11v10$time <- timing
# timing <- system.time({
#   glmmTMBm11v11 <- glmmTMB(WinOpenAction ~ Season + Room + sin((4*omega)*Hour) + sin((3*omega)*Hour) + sin((2*omega)*Hour) + sin((1*omega)*Hour) + (1+OutdoorTempadj+TempCadj+CO2C_logadj+SolRad_logadj|Dwelling),
#                           data = datClosed,family = binomial)
# }); glmmTMBm11v11$time <- timing
# timing <- system.time({
#   glmmTMBm11v12 <- glmmTMB(WinOpenAction ~ Season + Room + sin((4*omega)*Hour) + sin((3*omega)*Hour) + sin((2*omega)*Hour) + sin((1*omega)*Hour) + (1+OutdoorTempadj+TempCadj+CO2C_logadj+SolRad_logadj|Dwelling),
#                           data = datClosed,family = binomial)
# }); glmmTMBm11v12$time <- timing
timing <- system.time({
  glmmTMBm11v13 <- glmmTMB(WinOpenAction ~ Weekday + Season + Room + sin((4*omega)*Hour) + sin((3*omega)*Hour) + sin((2*omega)*Hour) + sin((1*omega)*Hour) + (1+OutdoorTempadj+TempCadj+CO2C_logadj+SolRad_logadj|Dwelling),
                           data = datClosed,family = binomial)
}); glmmTMBm11v13$time <- timing
# timing <- system.time({
#   glmmTMBm11v14 <- glmmTMB(WinOpenAction ~ Season + Room + sin((4*omega)*Hour) + sin((3*omega)*Hour) + sin((2*omega)*Hour) + sin((1*omega)*Hour) + (1+OutdoorTempadj+TempCadj+CO2C_logadj+SolRad_logadj|Dwelling),
#                           data = datClosed,family = binomial)
# }); glmmTMBm11v14$time <- timing
timing <- system.time({
  glmmTMBm11v15 <- glmmTMB(WinOpenAction ~ Group + Season + Room + sin((4*omega)*Hour) + sin((3*omega)*Hour) + sin((2*omega)*Hour) + sin((1*omega)*Hour) + (1+OutdoorTempadj+TempCadj+CO2C_logadj+SolRad_logadj|Dwelling),
                           data = datClosed,family = binomial)
}); glmmTMBm11v15$time <- timing

#######################
#BIC(glmmTMBm11r1)
#BIC(glmmTMBm11r2)
BIC(glmmTMBm11r3)
BIC(glmmTMBm11r4)
#BIC(glmmTMBm11r5)
BIC(glmmTMBm11r6)
BIC(glmmTMBm11r7)
#BIC(glmmTMBm11r8)
BIC(glmmTMBm11r9)

BIC(glmmTMBm11v1)
BIC(glmmTMBm11v2)
BIC(glmmTMBm11v3) #bedst
BIC(glmmTMBm11v4)
BIC(glmmTMBm11v5)
BIC(glmmTMBm11v6)
BIC(glmmTMBm11v7)
BIC(glmmTMBm11v8)
BIC(glmmTMBm11v9)
BIC(glmmTMBm11v10)
#BIC(glmmTMBm11v11)
#BIC(glmmTMBm11v12)
BIC(glmmTMBm11v13)
#BIC(glmmTMBm11v14)
BIC(glmmTMBm11v15)

timing <- system.time({
  glmmTMBm11v3_upd <- glmmTMB(WinOpenAction ~ OutdoorTemp + TempC + CO2C_log + SolRad_log
                              + RHC + Season + Room + sin((4*omega)*Hour) + sin((3*omega)*Hour) + sin((2*omega)*Hour) + sin((1*omega)*Hour) + (1+OutdoorTempadj+TempCadj+CO2C_logadj+SolRad_logadj|Dwelling),
                          data = datClosed,family = binomial)
}); glmmTMBm11v3_upd$time <- timing

BIC(glmmTMBm11v3_upd)

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

# no better model

#######################
#######################
#######################

