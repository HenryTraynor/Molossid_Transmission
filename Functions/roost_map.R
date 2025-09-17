library(truncnorm)
roostMap <- function(roost_parms) {
  num_roosts <- roost_parms["num_roosts"]
  num_clusters <- roost_parms["num_clusters"]
  sd <- roost_parms["sd"]
  avg_N_max <- roost_parms["avg_N_max"]
  min_N_max <- roost_parms["min_N_max"]
  clusterMap <- function(num_roosts, num_clusters, sd) {
    #Dataframe to hold roost locations
    roosts <- data.frame(x=vector("numeric", num_roosts),
                         y=vector("numeric", num_roosts))
    
    #Randomly sort roosts
    roosts <- cbind(roosts,sample.int(n=num_clusters, size=num_roosts, replace=TRUE))
    colnames(roosts) <- c("x","y","bin")
    
    #Assign locations to first 5 roosts and designate them as cluster centers
    for(i in 1:num_clusters) {
      roosts$x[i] <- runif(1,0,1)
      roosts$y[i] <- runif(1,0,1)
      roosts$bin[i] <- i
    }
    
    #Iterate through and rtruncnorm roosts arounds clusters
    for(i in (num_clusters+1):num_roosts) {
      bin_num = roosts$bin[i]
      cluster_center = roosts[bin_num,1:2]
      roosts$x[i] <- rtruncnorm(1, a=0, b=1, mean=cluster_center$x[1], sd)
      roosts$y[i] <- rtruncnorm(1, a=0, b=1, mean=cluster_center$y[1], sd)
    }
    roosts$bin <- as.character(roosts$bin)
    return(roosts)
  }
  
  roost_map <- cbind(clusterMap(roost_parms[1],
                                roost_parms[2],
                                roost_parms[3]),
                     as.data.frame(as.character(seq(1,roost_parms[1]))),
                     as.data.frame(sample.int(avg_N_max, size=roost_parms[1], replace=TRUE)+min_N_max)
                     
  )
  colnames(roost_map)[4:5] <- c("lbl", "N_max")
  return(roost_map)
}

distMatrix <- function(num_roosts, roost_map) {
  #Matrix to store distance values between roosts
  roost_dist <- matrix(nrow=num_roosts, ncol=num_roosts)
  colnames(roost_dist) <- as.character(seq(1:num_roosts))
  
  #Iterate through roosts and calculate distances between them, creates symmetric matrix with zero diagonal
  for(i in 1:num_roosts) {
    for(j in 1:num_roosts) {
      roost_dist[i,j] = sqrt((roost_map$x[j]-roost_map$x[i])^2+(roost_map$y[j]-roost_map$y[i])^2)
    }
  }
  return(roost_dist)
}
