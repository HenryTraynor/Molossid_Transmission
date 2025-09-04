movement <- function(roosts, num_roosts, phi_max, roost_dist, scores) {
  sample.vec <- function(x, ...) x[sample(length(x), ...)]
  
  #matrix storing num bats in each compartment that leave roost i
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
  
  for(i in 1:num_roosts) {
    phi <- phi_max*(1-scores[i])
    #Number leaving each compartment
    N_e <- rbinom(1, roosts$N[i], phi)
    emigrant_bats$N[i] <- N_e
    if(N_e > 0) {
      x <- c(rep(1, roosts$S[i]),
             rep(2, roosts$I[i]),
             rep(3, roosts$R[i]))
      samp <- sample.vec(x, N_e)
      for(j in 1:N_e) {
        emigrant_bats[i,samp[j]] <- emigrant_bats[i,samp[j]] + 1
      }
    }
    emigrant_bats$N[i] <- sum(emigrant_bats[i,1:3])
  }
  
  #Subtract these bats from home roost
  roosts[,1:4] <- roosts[,1:4] - emigrant_bats
  
  for(i in 1:num_roosts) {
    for(j in 1:4) {
    }
  }
  
  #Determine which roosts bats are traveling to and store in immigrant_bats
  for(i in 1:num_roosts) {
    #Decay function for j being destination
    #Exponential base may be an interesting way to alter dispersal distance
    #must add 0.1 so roosts can be recolonized
    pi <- (1/50)^roost_dist[i,]*(roosts$N/roosts$N_max+0.05)
    
    #for loop to set immigration zero for roosts at max occupancy and prevents returning to original roost
    for(k in 1:length(pi)) {
      if(roosts$N[k]>= roosts$N_max[k] || i==k) {
        pi[k] <- 0
      }
    }
    for(k in 1:3) {
      if(emigrant_bats[i,k] != 0) {
        #gives vector of destinations
        destinations <- sample.int(num_roosts,
                                   size=emigrant_bats[i,k],
                                   replace=TRUE,
                                   prob=pi)
        for(j in 1:length(destinations)) {
          immigrant_bats[destinations[j],k] <- immigrant_bats[destinations[j],k] + 1
        }
      }
    }
  }
  
  #Re-tally N for immigrants
  for(i in 1:num_roosts) {
    immigrant_bats$N[i] <- sum(immigrant_bats[i,1:3])
  }
  
  #Add immigrants to new roosts
  roosts[,1:4] <- roosts[,1:4] + immigrant_bats
  
  return(roosts)
}

#We want to sometimes remove roosts from the system
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
  pi <- (1/50)^roost_dist[roost_index,]*(roosts$N/roosts$N_max+0.05)
  for(k in 1:length(pi)) {
    if (roosts$N[k]>=roosts$N_max[k] || k==roost_index) {
      pi[k] <- 0
    }
  }
  for(k in 1:3) {
    if(emigrant_bats[k] != 0) {
      #gives vector of destinations
      destinations <- sample.int(num_roosts,
                                 size=emigrant_bats[k],
                                 replace=TRUE,
                                 prob=pi)
      for(j in 1:length(destinations)) {
        immigrant_bats[destinations[j],k] <- immigrant_bats[destinations[j],k] + 1
      }
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