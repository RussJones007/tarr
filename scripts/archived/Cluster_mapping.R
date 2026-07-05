## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----reading_layers, eval=TRUE------------------------------------------------
library(tarr,  quietly = TRUE)
library(sf,    quietly = TRUE)

# load and add city boundaries using readRDS and geo_sets
other_counties <- readRDS(file = geo_sets$surrounding_borders)
plot(x = st_geometry(other_counties), border = "grey60")

# load the Tarrant County border then add it to the map
tarr_border <- load_tarrant_spatial("tarrant_border")
plot(x = st_geometry(tarr_border), border = "red", add = TRUE )

rm(cities, other_counties, tarr_border)


## ----cases_to_sf--------------------------------------------------------------

# get theTarrant County outline
county  <- load_tarrant_spatial("tarrant_border")

# Select the fooflu cases and make it an sf POINTS object
foo <- synthetic_outbreak[synthetic_outbreak$condition == "fooflu",] |> 
  st_as_sf(coords = c("lon", "lat")) 
  
# foo needs a crs, assign it from the county sf object
st_crs(foo) = st_crs(county)
  
# plot the "cases"  and county outline
plot(st_geometry(foo), col = "red3")
plot(st_geometry(county), add = TRUE, border = "navyblue", col = NA)


## ----clusters, fig.width = 8, fig.height = 6----------------------------------
foo_scan <- scan_cluster(points = foo, method = "HDBSCAN", eps = 5280*1.25, minPts = 7)
foo_scan

# show the cases and clusters
plot(foo_scan)

