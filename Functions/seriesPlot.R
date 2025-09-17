# function for creating ggplot object from list of roosts

library(tidyr)
library(dplyr)
library(ggplot2)

seriesPlot <- function(roost_series, var) {
  roost_vec <- replicate(roost_parms[1], vector("numeric", length=length(roost_series)))
  for(i in 1:length(roost_series)) {
    for(j in 1:roost_parms[1]) {
      roost_vec[i,j] = roost_series[[i]][var][j,]
    }
  }
  
  colnames(roost_vec) <- paste0("Roost",as.character(seq(1:roost_parms[1])))
  roost_vec <- as.data.frame(roost_vec)
  roost_vec <- cbind(roost_vec,seq(1:length(roost_vec$Roost1)))
  colnames(roost_vec)[length(colnames(roost_vec))] <- "t"
  
  df_long <- roost_vec %>%
    pivot_longer(cols = starts_with("Roost"),
                 names_to = "Roost",
                 values_to = "var")
  
  p -> ggplot(df_long, aes(x=t, y=value,color=Roost)) +
    geom_line()
  
  return(p)
}
