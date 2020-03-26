library(mise)
mise()

# https://stackoverflow.com/questions/40039114/r-nlminb-what-does-false-convergence-actually-mean
# https://r.789695.n4.nabble.com/quot-False-convergence-quot-in-LME-td860675.html

source('Code_MC_Setup.R')
source('Code_MarkovChain_Fun.R')
dat_orig <- dat

# Choosing dwelling nr 1-16 (except the missing nr 2)
dat <- dat_orig[dat_orig$Dwelling==1,]

# GLM for Open and Closed
glm_open <- step(glm(WinOpenAction~1, data = dat[dat$WindowClosed==1,], family = binomial),
                 WinOpenAction ~ OutdoorTemp + Wind_log + OutdoorRH + SolRad_log
                 + SolTimer + TempC + RHC + LuxC_log + CO2C_log
                 + cos((1*omega)*Hour) + sin((1*omega)*Hour)
                 + Room + WeekDay + TimeOfDay + Season
                 , direction="both", k = log(length(dat[dat$WindowClosed==1,]$TempC)))

glm_closed <- step(glm(WinCloseAction~1, data = dat[dat$WindowClosed==0,], family = binomial),
                   WinOpenAction ~ OutdoorTemp + Wind_log + OutdoorRH + SolRad_log
                   + SolTimer + TempC + RHC + LuxC_log + CO2C_log
                   + cos((1*omega)*Hour) + sin((1*omega)*Hour)
                   + Room + WeekDay + TimeOfDay + Season
                   , direction="both", k = log(length(dat[dat$WindowClosed==0,]$TempC)))

cat("\014") # Clear console
glm_open$call
glm_closed$call

# The chosen model for Markov Chain Model
Model_O
Model_C

# Extract design matrix for model
X_O <- model.matrix(Model_O,data=dat)
X_C <- model.matrix(Model_C,data=dat)

dim(X_O)
dim(X_C)

# Transition propability matrix
Gamma <- matrix(0,2,2,byrow = T)

# Define y_t for data #c(open,closed)
y_t <- matrix(0,length(dat$WindowClosed),2,byrow = T)
for (t in 1:length(dat$WindowClosed)) {
  if (dat$WindowClosed[t] == 1){
    temp <- c(0,1)
  } else {temp <- c(1,0)}
  y_t[t,] <- temp
}

#params <- c(Model_O$coefficients,Model_C$coefficients)
params <- rep(0,dim(X_O)[2]+dim(X_C)[2])
nll <- nll.fun(params,X_O,X_C,Gamma,y_t);nll

timing <- system.time({
  opt <- nlminb(params,nll.fun,X_O=X_O,X_C=X_C,Gamma=Gamma,y_t=y_t,control=list(trace=1))
}); timing
opt
#params <- opt$par
#nll <- nll.fun(opt$par,X_O,X_C,Gamma,y_t);nll
opt$par[1:dim(X_O)[2]]
unname((coef(Model_O)))
opt$par[(dim(X_O)[2]+1):length(params)]
unname((coef(Model_C)))
logLik(Model_O)
logLik(Model_C)
-opt$objective

