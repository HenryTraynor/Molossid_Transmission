SIR.onestep <- function (x, params) {
  X <- x[2]
  Y <- x[3]
  Z <- x[4]
  N <- X+Y+Z
  beta <- params["beta"]
  mu <- params["mu"]
  gamma <- params["gamma"]
  
  ## each individual rate
  
  rates <- c(
    birth=mu*N,
    infection=beta*X*Y/N,
    recovery=gamma*Y,
    sdeath=mu*X,
    ideath=mu*Y,
    rdeath=mu*Z
  )
  
  #### what changes with each event?
  
  transitions <- list( 
    birth=c(1,0,0),
    infection=c(-1,1,0),
    recovery=c(0,-1,1),
    sdeath=c(-1,0,0),
    ideath=c(0,-1,0),
    rdeath=c(0,0,-1)
  )
  
  ## total event rate
  total.rate <- sum(rates)
  
  ## waiting time (note exponentially distributed random events)
  
  tau <- rexp(n=1,rate=total.rate)
  
  ## which event occurs?
  event <- sample.int(n=6,size=1,prob=rates/total.rate)
  x+c(tau,transitions[[event]])  ## double square bracket as recursive from list
}

############   Routine to define bounds of simulation

SIR.simul <- function (x, params, maxstep = 10000) {
  output <- array(dim=c(maxstep+1,4))
  colnames(output) <- names(x)
  output[1,] <- x
  k <- 1
  ## loop until either k > maxstep or
  ## there are no more infectives
  while ((k <= maxstep) && (x["Y"] > 0)) {
    k <- k+1
    output[k,] <- x <- SIR.onestep(x,params)
  }
  as.data.frame(output[1:k,])
}

#####   Routine to run program  ~ note use of plyr as rdply to store results

set.seed(56856583)
nsims <- 10
xstart <- c(time=0,X=396,Y=4,Z=0) #initial conditions
params <- c(mu=0.02,beta=60,gamma=365/13) #parameters

require(plyr) ###  plyr really powerful for simplifying repeated complex operations
simdat <- rdply(
  nsims,
  SIR.simul(xstart,params)
)
head(simdat)
plot(Y~time,data=simdat,type='n')
d_ply(simdat,".n",function(x)lines(Y~time,data=x,col=.n))
