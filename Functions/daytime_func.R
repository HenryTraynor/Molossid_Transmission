SIRleap <- function(x, SIR_parms, tau=1) {
  #tau is in 'nights'
  S <- x[1]
  I <- x[2]
  R <- x[3]
  N <- x[4]
  beta <- SIR_parms["beta"]
  gamma <- SIR_parms["gamma"]
  
  #c(new infections, recoveries)
  rates <- c(beta*S*I/N,
             gamma*I
  )
  rates <- rpois(n=length(rates), lambda=as.numeric(rates))
  x[1:3] <- x[1:3] + c(-1*rates[1], rates[1]-rates[2], rates[2])
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