nll.fun <- function(params,X_O,X_C,Gamma,y_t){
  n <- dim(X_C)[1]
  # Define parameters
  beta_O <- params[1:dim(X_O)[2]]
  beta_C <- params[(dim(X_O)[2]+1):length(params)]
  
  # Calculate gamma_11
  temp <- X_O %*% beta_O
  gamma_21 <- (exp(temp))/(1+exp(temp))

  # Calculate gamma_22
  temp <- X_C %*% beta_C
  gamma_12 <- (exp(temp))/(1+exp(temp))

  # Initialize log-Likelihood
  ll <- 0
  for (t in 1:(n-1)) {
    # Update transition probability matrix
    Gamma[1,1] <- 1-gamma_12[t]
    Gamma[1,2] <- gamma_12[t]
    Gamma[2,1] <- gamma_21[t]
    Gamma[2,2] <- 1-gamma_21[t]
    
    # Calculate next step, u_(t+1)
    u_t1 <- y_t[t,] %*% Gamma
    # Loglikelihood
    ll <- ll+log(sum(u_t1*y_t[t+1,]))
  }
  -ll
}
