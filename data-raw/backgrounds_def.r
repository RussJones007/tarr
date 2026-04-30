# -------------------------------------------------------------------------------------->
# Script: backgrounds_def.r  
# Description:
#   The script downloads and save background statics maps that may be used in plotting.
#   Sources for the maps include Google and Stadia Maps.  Both system require API keys.
#   Therefore, this script can be run only by person who has API keys for both systems.
#   
#   The map retrieval APIs are requested using EPSG:4326 coordinates.  However, all maps are returned from the servers
#   in EPSG:3857.  To plot on top of the map tile, trnasform points or polygons to crs = EPS:3857
#   
#   Note:  Maps have an inset for attribution purposes.  In the example of Google, the Google map logo is in the map,
#   lower left side.
# -------------------------------------------------------------------------------------->
# Author: Russ Jones
# Created: April 26, 2026
# -------------------------------------------------------------------------------------->
library(ggmap)

base_maps <- list(
  google = NULL,
  stadia = NULL
)


# Retrieve coordinates from the Tarrant County border object.  All coordinates are entered into the API calls as EPDG:4326
# However, all returned maps are in EPSG:3857
border         <- load_tarrant_spatial("tarrant_border")
tarrant_box    <- sf::st_bbox(border) |> set_names(c("left", "bottom", "right", "top"))
tarrant_center <- sf::st_centroid(border) |> st_geometry() |> unlist()

if (has_google_key()){
  
  # simplify call to
  retrieve_google <- partial(get_googlemap, 
                            center = tarrant_center, 
                            zoom = 10,
                            scale = 2) 
  
  google_types <- c(#"terrain", 
                    "roadmap" #, 
                    #"satellite",
                    #"hybrid"
                    )
  
  base_maps$google$color <- map(google_types, ~retrieve_google(maptype = .x, color = "color")) |> 
    set_names(google_types)
  
  base_maps$google$bw <- map(google_types, ~retrieve_google(maptype = .x, color = "bw")) |> 
    set_names(google_types)
  
 rm(google_types, retrieve_google) 
} else {
  warning("A Google API key was not found, therefore the Google map tiles were not retrieved\n",
          "You will need A Google account and an API key. \n",
          "See the Details section of the ggmap::register_google() function on how to get a key")
}


if(has_stadiamaps_key()){
  
  retrieve_stadia <- partial(get_stadiamap, bbox = tarrant_box, zoom = 11)
  stadia_types <- c(#"stamen_terrain", 
                    "stamen_toner_lite", 
                    "stamen_watercolor", 
                    "stamen_terrain_background", 
                    "stamen_terrain_labels" #, 
                    #"stamen_toner_lines",
                    #"stamen_toner_labels"
                    )
  
  base_maps$stadia <- map(stadia_types, ~ retrieve_stadia(maptype = .x, color = "color")) |> 
    set_names(stadia_types)
  
  rm(retrieve_stadia, stadia_types)
} else {
  warning("A Stadia Maps API key was not found, therefore the Stamen map tiles were not retrieved\n",
          "Review the details section for the ggmap::register_stadiamaps() function on how to get a key\n" )
}

# Openstreet Map temporairy not supportd
#base_maps$osm$color <- get_openstreetmap(bbox = tarrant_box, color = "color")

usethis::use_data(base_maps, internal = FALSE, overwrite = TRUE)
