# Converting excel sheet of Kenya field data to roost map

building_data <- read.csv("Kenya-building-location-occupancy.csv")
sites <- unique(building_data$site)
cat("Sites: ", sites)

rescale01 <- function(x) (x - min(x)) / (max(x) - min(x))

building_data$x <- rescale01(building_data$longitude)
building_data$y <- rescale01(building_data$latitude)

# Split the dataframe by 'site'
site_list <- split(building_data, building_data$site)

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
    grepl("modern", d$building_type, ignore.case = TRUE),
    round(rnorm(nrow(d), mean = 100, sd = 10)), # for modern roofs
    round(rnorm(nrow(d), mean = 20, sd = 5)) # for non-modern roofs
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

#---------------------------------------------------
#  SPATIAL STATS
#---------------------------------------------------

library(spatstat.geom)
library(spatstat.explore)
library(spdep)
library(dplyr)
library(stringr)

# Assume your combined dataframe is called building_data
# Columns: site, latitude, longitude, building_type, new_value, etc.

# Project to planar coordinates for distance-based stats
building_data_sf <- st_as_sf(building_data, coords = c("longitude", "latitude"), crs = 4326)
building_data_sf <- st_transform(building_data_sf, 32636)  # adjust UTM zone if needed
coords <- st_coordinates(building_data_sf)

# Add coordinates to original dataframe
building_data$X_proj <- coords[,1]
building_data$Y_proj <- coords[,2]

site_summary <- data.frame()

building_data$occ_estimate <- ifelse(
  grepl("modern", building_data$building_type, ignore.case = TRUE),
  round(rnorm(nrow(d), mean = 100, sd = 10)), # for modern roofs
  round(rnorm(nrow(d), mean = 20, sd = 5)) # for non-modern roofs
) 

# Loop over unique sites
for (site_name in unique(building_data$site)) {
  d <- building_data %>% filter(site == site_name)
  
  n_buildings <- nrow(d)
  prop_modern <- mean(grepl("modern", d$building_type, ignore.case = TRUE))
  
  if (n_buildings < 3) {
    # Not enough points for spatial stats
    mean_nn <- NA
    clark_evans_R <- NA
    morans_I <- NA
  } else {
    # Add tiny jitter to coordinates to avoid exact duplicates
    d$X_proj_jit <- jitter(d$X_proj, factor = 1e-6)
    d$Y_proj_jit <- jitter(d$Y_proj, factor = 1e-6)
    
    buffer <- 1e-6  # tiny buffer, same order as jitter
    
    # Spatstat point pattern
    win <- owin(
      xrange = range(d$X_proj_jit) + c(-buffer, buffer),
      yrange = range(d$Y_proj_jit) + c(-buffer, buffer)
    )
    
    # Use jittered coordinates in ppp()
    pp <- ppp(d$X_proj_jit, d$Y_proj_jit, window = win)
    
    # Mean nearest-neighbor
    mean_nn <- mean(nndist(pp))
    
    # Clark-Evans R
    clark_evans_R <- clarkevans(pp, correction = "none")
    
    # Moran's I (using new_value if available)
    if ("occ_estimate" %in% names(d)) {
      coords_jit <- cbind(d$X_proj_jit, d$Y_proj_jit)
      knn <- knearneigh(coords_jit, k = 4)
      nb <- knn2nb(knn)
      lw <- nb2listw(nb, style = "W", zero.policy = TRUE)
      morans_I <- moran(d$occ_estimate, lw, n = nrow(d), S0 = Szero(lw))$I
    } else {
      morans_I <- NA
    }
  }
  
  # Add to summary dataframe
  site_summary <- rbind(site_summary, data.frame(
    site = site_name,
    n_buildings = n_buildings,
    prop_modern = prop_modern,
    mean_nn = mean_nn,
    clark_evans_R = clark_evans_R,
    morans_I = morans_I
  ))
}

# View final summary
print(site_summary)
