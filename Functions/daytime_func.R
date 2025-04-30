SIRleap <- function(y, SIR_parms) {
  S <- y$S
  I <- y$I
  R <- y$R
  N <- y$N
  beta <- SIR_parms["beta"]
  gamma <- SIR_parms["gamma"]
  if(N<=0) {
    prob <- c(0,
              1-exp(-1*gamma))
  }
  else {
    prob <- c(1-exp(-1*beta*S*I/N),
              1-exp(-1*gamma))
  }
  num <- c(rbinom(1,S,prob[1]),
           rbinom(1,I,prob[2]))
  
  y[1:3] <- y[1:3] + c(-1*num[1], num[1]-num[2], num[2])
  for(i in 1:length(y)) {
    if(y[i] < 0 || is.na(y[i])) {
      browser()
      #y[i] <- 0
    }
  }
  return(y)
}

#applies SIRleap() to all roosts
#model arg will become non-default to allow for changes in model compartments
daytime <- function(roosts, roost_parms, SIR_parms, model="SIR") {
  if(model=="SIR") {
    for(i in 1:roost_parms[1]) {
      y <- roosts[i,1:4]
      y <- SIRleap(y, SIR_parms)
      roosts[i,1:4] <- y
      for(j in 1:length(y)) {
        if(y[j]< 0 || is.na(y[j])) {
          browser()
        }
      }
    }
  }
  return(roosts)
}

roost_series <- list()
roost_series[[1]] <- roosts

for(i in 2:500) {
  roost_series[[i]] <- daytime(roost_series[[i-1]], roost_parms, SIR_parms, model="SIR")
}
