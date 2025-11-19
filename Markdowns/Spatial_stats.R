files_functions <- list.files(path = "C:/Users/henry/Molossid_Transmission/Functions", pattern = "\\.R$", full.names = TRUE)
for(file in files_functions) {
  source(file)
}

epidemic_results <- list()

SIR_parms  <- c(beta = 0.05, #transmission rate
                gamma = 0.03) # recovery rate

for(name in names(site_list)) {
  roost_map <- site_list[[name]]
  roost_parms <- c(num_roosts = nrow(roost_map),  # number of roosts
                   num_clusters = 3, # number of clusters -- doesnt matter here
                   sd = 0.1)           # standard distance from cluster center -- doesnt matter here
  
  roost_dist <- list_pairwise_distances[[name]]
  
  roosts <- data.frame(
    S = vector("numeric", length=nrow(roost_map)),
    I = vector("numeric", length=nrow(roost_map)),
    R = vector("numeric", length=nrow(roost_map)),
    N = vector("numeric", length=nrow(roost_map)),
    N_max = roost_map$occ_estimate
  )
  
  #Populate Roosts
  roosts$N <- round(roosts$N_max/2)
  roosts$S <- roosts$N
  roosts[1,1:2] <- c(roosts$S[1]-1,1)
  
  #Total distances between any 2 roosts
  total_dist <- 0
  for(i in 1:roost_parms[1]) {
    for(j in 1:i) {
      total_dist <- total_dist + as.numeric(roost_dist[i,j])
    }
  }
  
  #Score, a measure of isolation. greater score ==> greater isolation
  scores <- vector("numeric", length=roost_parms[1])
  for(i in 1:roost_parms[1]) {
    tot <- sum(roost_dist[i,])
    scores[i] <- tot/total_dist
  }
  
  roost_series_series <- list()
  
  num_realizations <- 10
  num_days <- 150
  
  for(i in 1:num_realizations) {
    roost_series <- list()
    roost_series[[1]] <- roosts
    for(j in 2:num_days) {
      roost_series[[j]] <- singleDay(roost_series[[j-1]],
                                     roost_dist,
                                     roost_parms, 
                                     SIR_parms,
                                     model="SIR",
                                     scores)
    }
    roost_series_series[[i]] <- roost_series
  }
  
  epidemic_results[[name]] <- roost_series_series
}


#-----------------
# TIDY
#---------------

library(dplyr)
library(purrr)
library(tidyr)
library(ggplot2)

# Flatten the nested structure
epidemic_df <- map_df(names(epidemic_results), function(site_name) {
  site_data <- epidemic_results[[site_name]]
  
  # Each site has 10 realizations
  map_df(seq_along(site_data), function(run_id) {
    realization <- site_data[[run_id]]
    
    # Each realization is a list of 150 daily dataframes
    map_df(seq_along(realization), function(day_id) {
      day_df <- realization[[day_id]]
      
      tibble(
        site = site_name,
        run = run_id,
        day = day_id,
        infected = sum(day_df$I, na.rm = TRUE)  # or day_df$I etc.
      )
    })
  })
})


ggplot(epidemic_df, aes(x = day, y = infected, group = run, color = run)) +
  geom_line(alpha = 0.7) +
  facet_wrap(~site, scales = "free_y") +
  theme_minimal() +
  theme(legend.position = "none") +
  labs(
    title = "Epidemic simulations by site",
    x = "Day",
    y = "Infected individuals"
  )


#-----------
# STAT EXTRACT
#-----------

# Function to check if a realization has any infected individuals
has_infection <- function(realization_list) {
  # Flatten all daily infected counts
  infected_total <- map_dbl(realization_list, ~ sum(.x$infected, na.rm = TRUE))
  any(infected_total > 0)
}

# Filter epidemic_results
epidemic_results <- map(epidemic_results, function(site_data) {
  # Keep only realizations with at least one infected individual
  keep <- map_lgl(site_data, has_infection)
  site_data[keep]
})

# Function to compute peak proportion of infected for a single realization
get_peak_prop <- function(realization_list) {
  # realization_list: list of 150 daily dataframes
  daily_props <- map_dbl(realization_list, function(day_df) {
    sum(day_df$I, na.rm = TRUE) / sum(day_df$N, na.rm = TRUE)
  })
  max(daily_props)
}

# Loop over sites to summarize
site_summary_epidemic <- map_df(names(epidemic_results), function(site_name) {
  site_data <- epidemic_results[[site_name]]  # list of 10 realizations
  
  # Compute peak proportion for each realization
  peak_props <- map_dbl(site_data, get_peak_prop)
  
  tibble(
    site = site_name,
    mean_peak_prop = mean(peak_props),
    sd_peak_prop = sd(peak_props),
    n_realizations = length(peak_props)
  )
})

site_summary_epidemic

# Alphabetize by site
site_summary <- site_summary %>%
  arrange(site)
# Remove the last row
site_summary <- site_summary %>%
  slice(1:(n() - 1))

library(stringr)
site_summary_epidemic <- site_summary_epidemic %>%
  mutate(site = str_remove(site, "^site_"))

combined_summary <- site_summary_epidemic %>%
  left_join(site_summary, by = "site")




ggplot(combined_summary, aes(x = site, y = mean_peak_prop)) +
  geom_col(fill = "steelblue", alpha = 0.7) +
  geom_errorbar(aes(ymin = mean_peak_prop - sd_peak_prop,
                    ymax = mean_peak_prop + sd_peak_prop),
                width = 0.2, color = "black") +
  theme_minimal() +
  labs(
    title = "Peak Proportion of Infected Individuals by Site",
    x = "Site",
    y = "Mean Peak Proportion Infected"
  )


ggplot(combined_summary, aes(x = site, y = mean_peak_prop)) +
  geom_col(fill = "steelblue", alpha = 0.7) +
  geom_errorbar(aes(ymin = mean_peak_prop - sd_peak_prop,
                    ymax = mean_peak_prop + sd_peak_prop),
                width = 0.2, color = "black") +
  geom_text(aes(label = paste0("NN=", round(mean_nn, 2),
                               "\nCE=", round(clark_evans_R, 2),
                               "\nPM=", round(prop_modern, 2))),
            vjust = -0.5, size = 3) +
  theme_minimal() +
  labs(
    title = "Peak Proportion of Infected Individuals with Spatial Statistics",
    x = "Site",
    y = "Mean Peak Proportion Infected"
  )

ggplot(combined_summary, aes(x = mean_nn, y = mean_peak_prop, label = site)) +
  geom_point(size = 3, color = "steelblue") +
  geom_text(vjust = -0.5, hjust = 0.5) +
  geom_errorbar(aes(ymin = mean_peak_prop - sd_peak_prop,
                    ymax = mean_peak_prop + sd_peak_prop),
                width = 0.1, color = "black") +
  theme_minimal() +
  labs(
    title = "Peak proportion infected vs. Mean nearest-neighbor",
    x = "Mean nearest-neighbor distance",
    y = "Mean peak proportion infected"
  )

combined_long <- combined_summary %>%
  pivot_longer(cols = c(mean_nn, clark_evans_R, prop_modern),
               names_to = "spatial_stat",
               values_to = "stat_value")

ggplot(combined_long, aes(x = stat_value, y = mean_peak_prop, color = spatial_stat)) +
  geom_point(size = 3) +
  geom_text(aes(label = site), vjust = -0.5, hjust = 0.5, show.legend = FALSE) +
  geom_errorbar(aes(ymin = mean_peak_prop - sd_peak_prop,
                    ymax = mean_peak_prop + sd_peak_prop),
                width = 0.05) +
  facet_wrap(~spatial_stat, scales = "free_x") +
  theme_minimal() +
  labs(
    title = "Peak proportion infected vs. Spatial statistics",
    x = "Spatial statistic value",
    y = "Mean peak proportion infected"
  )

# Calculate correlations for each spatial statistic
cor_values <- combined_long %>%
  group_by(spatial_stat) %>%
  summarize(corr = cor(stat_value, mean_peak_prop, use = "complete.obs")) %>%
  ungroup()

# Merge correlation values back for annotation
combined_long <- combined_long %>%
  left_join(cor_values, by = "spatial_stat")

# Plot with regression line and correlation
ggplot(combined_long, aes(x = stat_value, y = mean_peak_prop)) +
  geom_point(aes(color = spatial_stat), size = 3) +
  geom_text(aes(label = site), vjust = -0.5, hjust = 0.5, show.legend = FALSE) +
  geom_errorbar(aes(ymin = mean_peak_prop - sd_peak_prop,
                    ymax = mean_peak_prop + sd_peak_prop),
                width = 0.05) +
  geom_smooth(method = "lm", se = FALSE, color = "black") +
  facet_wrap(~spatial_stat, scales = "free_x") +
  geom_text(
    data = cor_values,
    aes(x = Inf, y = Inf, label = paste0("r = ", round(corr, 2))),
    inherit.aes = FALSE,
    hjust = 1.1, vjust = 1.1
  ) +
  theme_minimal() +
  labs(
    title = "Peak proportion infected vs. Spatial statistics",
    x = "Spatial statistic value",
    y = "Mean peak proportion infected"
  )
