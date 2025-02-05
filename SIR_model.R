SIR <- function(t, y, parms){
  #t is the times for which I want the solution. It is a vector
  #y is a vector with as many values as there are variables in the model
  #parameters is a vector of the parameter values
  S <- y[1]
  I <- y[2]
  R <- y[3]
  N <- S+I+R
  
  a <- parms[1]
  b <- parms[2]
  beta <- parms[3]
  nu <- parms[4]
  alpha <- parms[5]
  gamma <- parms[6]
  
  dy <- numeric(3) #vector called dy, which will contain the three equations
  dy[1] <- a*N-b*S-beta*S*I+nu*R
  dy[2] <- beta*S*I-I*(b+alpha+gamma)
  dy[3] <- gamma*I-R*(b+nu)
  
  return(list(dy))
}