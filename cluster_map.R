library(truncnorm)
cluster_map <- function(num_roosts, num_clusters, sd) {
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
