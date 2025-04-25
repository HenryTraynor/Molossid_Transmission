movement <- function(roosts, num_roosts, phi_max, roost_dist) {
  num_roosts <- roost_parms[1]
  
  #matrix that stores total number of bats leaving roost i
  emigrant_bats <- data.frame(
    S = vector("numeric", length=num_roosts),
    I = vector("numeric", length=num_roosts),
    R = vector("numeric", length=num_roosts),
    N = vector("numeric", length=num_roosts)
  )
  #matrix storing num bats that travel to roost i
  immigrant_bats <- data.frame(
    S = vector("numeric", length=num_roosts),
    I = vector("numeric", length=num_roosts),
    R = vector("numeric", length=num_roosts),
    N = vector("numeric", length=num_roosts)
  )
  phi_max=0.25
  for(i in 1:num_roosts) {
    phi <- phi_max*(1-scores[i])
    #Number from each compartment leaving
    N_i <-  rbinom(1, roosts$N[i], phi)
    emigrant_bats$N[i] <- N_i
    #x is stored to determine list of probablities when drawing bats from compartments w/o replacement
    x <- as.numeric(roosts[i,1:3])
    for(j in 1:N_i) {
      #if statement so we dont draw from zero bats
      if(!all(x==0)) {
        k <- sample.int(3, 1, prob=x)
        emigrant_bats[i,k] <- emigrant_bats[i,k] + 1
        #update roost values for prob.
        x[k] <- x[k]-1
      }
    }
    #update N
    emigrant_bats$N[i] <- sum(emigrant_bats[i,1:3])
  }
  
  #Bats going from i to j
  for(i in 1:num_roosts) {
    #Decay function for j being destination
    #Exponential base may be an interesting way to alter dispersal distance
    pi <- (1/50)^roost_dist[i,]*(roosts$N/roosts$N_max+0.05) #must add 0.1 so roosts can be recolonized
    for(k in 1:length(pi)) {
      if (roosts$N[k]>=roosts$N_max[k]) {
        pi[k]=0
      }
    }
    #so that bat to not return to the same roost
    pi[i]=0
    #if size==0, dont sample and set destination to 0
      if(emigrant_bats$S[i] !=0 && !is.na(emigrant_bats$S[i])) {
        destinationS <- sample.int(num_roosts, size=emigrant_bats$S[i], replace=TRUE, prob=pi)
        
        for(j in 1:length(destinationS)) {
          immigrant_bats$S[destinationS[j]] <- immigrant_bats$S[destinationS[j]] + 1
        }
      }
      if(emigrant_bats$I[i] !=0 && !is.na(emigrant_bats$I[i])) {
        destinationI <- sample.int(num_roosts, size=emigrant_bats$I[i], replace=TRUE, prob=pi)
        
        for(j in 1:length(destinationI)) {
          immigrant_bats$I[destinationI[j]] <- immigrant_bats$I[destinationI[j]] + 1
        }
      }
      if(emigrant_bats$R[i] !=0 && !is.na(emigrant_bats$R[i])) {
        destinationR <- sample.int(num_roosts, size=emigrant_bats$R[i], replace=TRUE, prob=pi)
        
        for(j in 1:length(destinationR)) {
          immigrant_bats$R[destinationR[j]] <- immigrant_bats$R[destinationR[j]] + 1
        }
      }
    
  }
  for(i in 1:num_roosts) {
    immigrant_bats$N[i] <- sum(immigrant_bats[i,1:3])
  }
  
  #Subtract emigrants
  roosts[,1:4] <- roosts[,1:4] - emigrant_bats
  #Add immigrants
  roosts[,1:4] <- roosts[,1:4] + immigrant_bats
  
  return(roosts)
}

b#We want to sometimes remove roosts from the system
#Determine where these bats go

deleteRoost <- function(roosts, roost_index) {
  num_roosts <- roost_parms[1]
  #Bats leaving 'roost_index'
  emigrant_bats <- roosts[roost_index,1:4]
  #Bat destinations
  immigrant_bats <- data.frame(
    S = vector("numeric", length=num_roosts),
    I = vector("numeric", length=num_roosts),
    R = vector("numeric", length=num_roosts),
    N = vector("numeric", length=num_roosts)
  )
  
  #Decay function for j being destination
  #Exp base may be an interesting way to alter dispersal distance
  pi <- (1/100)^roost_dist[roost_index,]*(roosts$N/roosts$N_max)
  for(k in 1:length(pi)) {
    if (roosts$N[k]>=roosts$N_max[k]) {
      pi[k]=0
    }
  }
  pi[roost_index]=0
  #if size==0, dont sample and set destination to 0
  if(emigrant_bats$S !=0 && !is.na(emigrant_bats$S)) {
    destinationS <- sample.int(num_roosts, size=emigrant_bats$S, replace=TRUE, prob=pi)
    
    for(j in 1:length(destinationS)) {
      immigrant_bats$S[destinationS[j]] <- immigrant_bats$S[destinationS[j]] + 1
    }
  }
  if(emigrant_bats$I !=0 && !is.na(emigrant_bats$I)) {
    destinationI <- sample.int(num_roosts, size=emigrant_bats$I, replace=TRUE, prob=pi)
    
    for(j in 1:length(destinationI)) {
      immigrant_bats$I[destinationI[j]] <- immigrant_bats$I[destinationI[j]] + 1
    }
  }
  if(emigrant_bats$R !=0 && !is.na(emigrant_bats$R)) {
    destinationR <- sample.int(num_roosts, size=emigrant_bats$R, replace=TRUE, prob=pi)
    
    for(j in 1:length(destinationR)) {
      immigrant_bats$R[destinationR[j]] <- immigrant_bats$R[destinationR[j]] + 1
    }
  }
  
  for(i in 1:num_roosts) {
    immigrant_bats$N[i] <- sum(immigrant_bats[i,1:3])
  }
  
  #Subtract emigrants
  roosts[roost_index,] <- c(0,0,0,0,0)
  #Add immigrants
  roosts[,1:4] <- roosts[,1:4] + immigrant_bats
  
  return(roosts)
}