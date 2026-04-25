# develop2.r
# Load libraries
library(sf)
library(dplyr)
library(gstat)
library(ggplot2)

# Read clustered data
df <- read.csv("dbscan_clustered_points.csv")

# Separate lon/lat from geometry string
df <- df %>%
  mutate(
    lon = as.numeric(sub(".*\\((.*) .*", "\\1", geometry)),
    lat = as.numeric(sub(".* (.*)\\)", "\\1", geometry))
  )

# Convert to sf
pts <- st_as_sf(df, coords = c("lon", "lat"), crs = 4326) %>%
  st_transform(2276)  # feet projection

# Simulate a value to interpolate (e.g., population, cases)
set.seed(123)
pts$value <- runif(nrow(pts), 10, 100)

# Interpolate each cluster
all_surfaces <- list()

for (cl in unique(pts$dbscan_cluster)) {
  if (cl == -1) next  # Skip noise
  
  pts_cl <- pts %>% filter(dbscan_cluster == cl)
  if (nrow(pts_cl) < 3) next
  
  # Create convex hull
  hull <- st_convex_hull(st_union(pts_cl))
  
  # Create grid over the hull
  grid <- st_make_grid(hull, cellsize = 1000) %>% st_intersection(hull)
  grid <- st_sf(geometry = grid)
  
  # IDW interpolation
  idw_model <- gstat::idw(value ~ 1, pts_cl, newdata = grid, idp = 2)
  idw_model$cluster <- as.factor(cl)
  all_surfaces[[as.character(cl)]] <- idw_model
}

# Combine all surfaces
interpolated <- do.call(rbind, all_surfaces)

# Plot
ggplot() +
  geom_sf(data = interpolated, aes(fill = var1.pred), color = NA) +
  geom_sf(data = pts, size = 1, color = "black") +
  scale_fill_viridis_c() +
  facet_wrap(~cluster) +
  theme_minimal() +
  ggtitle("Interpolated Values within DBSCAN Clusters (IDW)")
