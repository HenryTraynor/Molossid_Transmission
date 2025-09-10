#Completes movement of bats and intra-roost transmission in one line
singleDay <- function(roosts, roost_dist, roost_parms, inf_parms, model, scores, phi_max=0.25) {
  new_roosts <- movement2(roosts, num_roosts=roost_parms[1], phi_max=0.25, roost_dist, scores)
  new_roosts <- daytime(new_roosts, roost_parms, inf_parms, model="SIR")
  return(new_roosts)
}
