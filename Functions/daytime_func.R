SIRleap <- function(x, SIR_parms) {
  S <- x$S
  I <- x$I
  R <- x$R
  N <- x$N
  beta <- SIR_parms["beta"]
  gamma <- SIR_parms["gamma"]
  
  prob <- c(1-exp(-1*beta*S*I/N),
            1-exp(-1*gamma))
  num <- c(rbinom(1,S,prob[1]),
           rbinom(1,I,prob[2]))
  
  x[1:3] <- x[1:3] + c(-1*num[1], num[1]-num[2], num[2])
  return(x)
}

#applies SIRleap() to all roosts
#model arg will become non-default to allow for changes in model compartments
daytime <- function(roosts, roost_parms, SIR_parms, model="SIR") {
  if(model=="SIR") {
    for(i in 1:roost_parms[1]) {
      x <- roosts[i,1:4]
      x <- SIRleap(x, SIR_parms)
      roosts[i,1:4] <- x
    }
  }
  return(roosts)
}