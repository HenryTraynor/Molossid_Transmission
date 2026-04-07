SIRleap <- function(y, SIR_parms) {
  S <- y$S
  I <- y$I
  R <- y$R
  N <- y$N
  beta <- SIR_parms["beta"]
  gamma <- SIR_parms["gamma"]
  if(N<=0) {
    prob <- c(0,
              0# 1-exp(-1*gamma)
              )
  }
  else {
    prob <- c(1-exp(-1*beta*S*I/N),
              1-exp(-1*gamma))
  }
  num <- c(rbinom(1,S,prob[1]),
           rbinom(1,I,prob[2]))
  
  y[1:3] <- y[1:3] + c(-1*num[1], num[1]-num[2], num[2])
  return(y)
}

SIRSleap <- function(y, SIRS_parms) {
  S <- y$S
  I <- y$I
  R <- y$R
  N <- y$N
  beta <- SIRS_parms["beta"]
  gamma <- SIRS_parms["gamma"]
  omega <- SIRS_parms["omega"]  # loss of immunity
  
  if (N <= 0) {
    prob <- c(0,
              0,
              0)
  } 
  else {
    prob <- c(1 - exp(-1*beta*S*I/N),  # infection
              1 - exp(-1*gamma),             # recovery
              1 - exp(-1*omega))             # loss of immunity
  }
  
  num <- c(rbinom(1, S, prob[1]),  # S -> I
           rbinom(1, I, prob[2]),  # I -> R
           rbinom(1, R, prob[3]))  # R -> S
  
  y[1:3] <- y[1:3] + c(
    -num[1] + num[3],   # S gains from R, loses to I
    num[1] - num[2],   # I gains from S, loses to R
    num[2] - num[3]    # R gains from I, loses to S
  )
  
  return(y)
}


#applies SIRleap() to all roosts
#model arg will become non-default to allow for changes in model compartments
daytime <- function(roosts, roost_parms, inf_parms, model) {
  if(model=="SIR") {
    for(i in 1:roost_parms[1]) {
      y <- roosts[i,1:4]
      y <- SIRleap(y, inf_parms)
      roosts[i,1:4] <- y
    }
  }
  if(model=="SIRS") {
    for(i in 1:roost_parms[1]) {
      y <- roosts[i,1:4]
      y <- SIRSleap(y, inf_parms)
      roosts[i,1:4] <- y
    }
  }
  return(roosts)
}


