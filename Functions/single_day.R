#Completes movement of bats and intra-roost transmission in one line
singleDay <- function(roosts, roost_dist, roost_parms, inf_parms, model, scores, phi_max=0.25) {
  # Defensive sanitation
  roosts[is.na(roosts)] <- 0
  roosts$N[roosts$N < 0 | is.na(roosts$N)] <- 0
  roosts$N_max[is.na(roosts$N_max) | roosts$N_max <= 0] <- 1
  
  new_roosts <- movement2(
    roosts = roosts,
    num_roosts = roost_parms[1],
    phi_max = phi_max,
    roost_dist = roost_dist,
    scores = scores
  )
  
  # Daytime infection process
  new_roosts <- daytime(new_roosts, roost_parms, inf_parms, model = "SIR")
  return(new_roosts)
}
