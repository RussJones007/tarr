# =====================================================================================
# geo_define.r
#
# Defines access to commonly used geographic data sets for Tarrant County.  All
# of the data sets are returned as simple features by default.  All are returned
# with a CRS of epsg of 4326 which is used by Google for geocoding. The data
# sets are read from the epi shared drive as "RDS" files rather than being
# stored in the package itself. This introduces an external dependency that can
# break the function that loads the requested dataset if the required files are
# moved. This just stores a list in the package of the names and locations of
# the data sets as rds files for use in the internal package function
# load_tarrant_spatial().
#
# Data sets include:
# 1. County borders including surounding counties
# 2. Tarrant county border alone
# 3. School districts
# 4. Cities boundaries
# 5. Zip code tabulation areas (ZCTA)
# 6. Census tracts
# 7. County ggmap backgrounds - not implemented
#   a. b/w tone - not implemented
#   b. color streets - not implemented
#   c. color terrain - not implemented
# 8.the bounding box for the county - not implemented
#
#  If the path$spatial  location changes, then  this defintion file should be rerun
#
# created 6/11/2019
# R Jones
# =====================================================================================
library(sf)
library(ggmap)

# define the original geodata sets by name and location
geo_sets <- list(
  "ZCTA"  = list(orig = "zips.tarrant",
                          path   = file.path(paths$spatial,"Tarrant/Zip Code Tabluation Areas/Tarrant Zip Codes.rdata")),
  "tarrant_border"   = list(orig = "tarr.border",
                          path   = file.path(paths$spatial,"Tarrant/Border/Tarrant Border.rdata")),
  "tarrant_tracts"   = list(orig = "TarrTracts",
                          path   = file.path(paths$spatial,"Tarrant/Tarrant Census Tracts.rdata")),
  "surrounding_borders"  = list(orig = "",
                           path  = file.path(paths$spatial, "Tarrant and Surrounding Counties Borders/County_2015.shp")),
  "tarrant_blocks"   = list(orig = "Tarr.bg10",
                          path   = file.path(paths$spatial, "Tarrant/Tarrant Block Groups.rdata")),
  "tarrant_cities"   = list(orig = "",
                          path   = file.path(paths$spatial, "Tarrant/Cities/Cities_2015.shp")),
  "isd"              = list(orig = "schools.tarrant",
                          path   = file.path(paths$spatial, "Tarrant/ISD/School Districts.rdata"))
)

# check that all file paths are correct
pths <- purrr::map_lgl(geo_sets, function(x) file.exists(x$path))
#geo_sets[!pths]

# if any pths are FALSE, stop execution and print out the error.
if(any(pths==FALSE)){
  stop("Missing file path for geo_sets:", pths[pths==FALSE])
}


# process the geodata sets to make sure they are available as rds files
# save_rds takes an item and ensures any shape files have a saved rds file in the same folder
save_rds <- function(x){
  fn <- paste0(dirname(x$path),"/",tools::file_path_sans_ext(basename(x$path)),".rds")
  if(tools::file_ext(x$path) == "shp" & file.exists(x$path)){
    #if(!file.exists(fn)){
      geo = st_read(x$path) %>%
        st_transform(crs = 4326)
    #}
    x <- fn   # change item to reflect rds file existence
  }else if(tools::file_ext(x$path) == "rdata" & file.exists(x$path)){
    load(x$path)
    if(!exists(x$orig)){
      print(paste( x$orig, "is not the original varibale name for", x$path))
    }else {
    geo <- get(x$orig) %>%
       st_as_sf() %>%
       st_transform(crs = 4326)
    }
  }
#  print(paste("Saving", "to", fn))
  saveRDS(object = geo,file = fn)
  ret = fn
  return(ret)
}

geo_sets <- purrr::map(geo_sets,save_rds)
rm(save_rds, pths)

# chnge the extnesion in the past for geo_sets to "rds"
#geo_sets <- map(geo_sets, \(item) str_replace(item$path, "(rdata|shp)$", "rds"))

usethis::use_data(geo_sets, overwrite = TRUE)
