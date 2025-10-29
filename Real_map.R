# Converting excel sheet of Kenya field data to roost map

building_data <- read.csv("Kenya-building-location-occupancy.csv")
sites <- unique(building_data$site)
cat("Sites: ", sites)

rescale01 <- function(x) (x - min(x)) / (max(x) - min(x))

building_data$x <- rescale01(building_data$longitude)
building_data$y <- rescale01(building_data$latitude)

# Split the dataframe by 'site'
site_list <- split(building_data, building_data$site)

# Next we need to rescale the lat/long to map onto [0,1]
rescale01 <- function(x) (x - min(x)) / (max(x) - min(x))
site_list <- lapply(site_list, function(d) {
  d$y  <- rescale01(d$latitude)
  d$x <- rescale01(d$longitude)
  return(d)
})


names(site_list) <- paste0("site_", make.names(names(site_list)))

# Assign each split dataframe to its own variable
list2env(site_list, envir = .GlobalEnv)

list_pairwise_distances <- lapply(site_list, function(d) {
  return(distMatrix(length(d$rownum),d))
})

library(ggplot2)
# Landscape
ggplot(building_data, aes(x,y, color = site)) +
  geom_point(size = 0.75, alpha = 0.8) +
  theme_minimal() +
  labs(
    title = "Scaled Roost Locations by Site",
    x = "Scaled Longitude",
    y = "Scaled Latitude"
  ) +
  coord_equal()  # keeps aspect ratio square

# Loop over each site dataframe in df_list
for (name in names(site_list)) {
  # Current site dataframe
  df_site <- site_list[[name]]
  
  # Create plot
  p <- ggplot(df_site, aes(x, y, color = building_type)) +
    geom_point(size = 3, alpha = 0.8) +
    theme_minimal() +
    labs(
      title = paste("Scaled Roost Locations of", name),
      x = "Scaled Longitude",
      y = "Scaled Latitude",
      color = "Building Type"
    ) +
    coord_equal()
  
  # Save plot to file (PNG)
  ggsave(
    filename = paste0("Site Maps/", name, ".png"),
    plot = p,
    width = 6, height = 5, dpi = 300
  )
}


# Apply to all site dataframes
site_list <- lapply(site_list, function(d) {
  d$occ_estimate <- ifelse(
    grepl("flat", d$roof_design, ignore.case = TRUE),
    round(rnorm(nrow(d), mean = 20, sd = 5)), # for flat roofs
    round(rnorm(nrow(d), mean = 100, sd = 10)) # for triangular roofs
  ) 
  return(d)
})

# (Optional) Reassign them to the global environment again
list2env(site_list, envir = .GlobalEnv)


library(mapview)
library(sf)

buildings_sf <- st_as_sf(
  building_data,
  coords = c("longitude", "latitude"),  # use real columns here
  crs = 4326   # WGS84 (lat/lon)
)

mapview(buildings_sf, zcol = "building_type", legend = TRUE)
