# ===========================================================================
# Geo_sets.r
# Documentation for geo_sets variable in the package
# ===========================================================================

# Document the geo_sets list
#' List of commonly used spatial data sets.
#'
#' A named list of spatial datasets that are often used for Tarrant County
#' mapping purposes. The name of the data set may be passed to the
#' load_tarrant_spatial() function which  returns the dataset and
#' optionally sets it to a user specififed CRS.
#' You may also use "geo_sets$name" to return the path to the *.rds file that
#' contains the data, then load it with readRDS.
#' All of these data sets are stored with a CRS=epsg:4326
#'
#' @format a named list of spatial datasets, all stored in crs = 4326
#'\itemize{
#'  \item tarrant_border: the county border
#'  \item surrounding_borders:  borders of counties surrounding Tarrant
#'  \item ZCTA:  Zip Code Tabulation Areas
#'  \item tarrant_cities:  City boundaries within Tarant County
#'  \item isd:  Boundaries of school districts for Tarrant County
#'  \item tarrant_tracts:  census tracts for Tarrant County
#'  \item tarrant_blocks:  census blocks for Tarrant County
#'}
#' @seealso \code{\link{load_tarrant_spatial}}
"geo_sets"
