#Sourcing
source("Functions/daytime_func.R")
source("Functions/movement_func.R")

#Completes movement of bats and intra-roost transmission in one line
singleDay <- function(roosts, roost_dist, roost_parms, SIR_parms, num_roosts, phi_max=0.25) {
  roosts <- movement(roosts, num_roosts=roost_parms[1], phi_max=0.25, roost_dist)
  roosts <- daytime(roosts, roost_parms, SIR_parms)
  return(roosts)
}