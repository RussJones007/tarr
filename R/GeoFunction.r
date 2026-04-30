# ======================================================================================>
# GeoFunction.r
# Originally these functions were used with the  "OneDisease Cluster.r: script
# However they are generalized to work other data frames by naming the needed columns and
# placed in the tarr package.
# Functions:
# makeAddress - takes fields and returns a vector of formatted addresses.  
# geoCases (deprecated) - takes two dataframes with addressees, ID , and lat lon fields 
# compares them and geocodes those that are missing - removed from package
# scanCluster - uses DBSCAN to ID cluster in a SpatialPointsDataframe
# mapCluster takes the identified clusters from scanCluster and maps them using ggplot and ggmap
## mapTheme object is a preset theme for maps, based on ggthemes:tufte
# 
# 
# By R Jones
# Created August 1, 2017
# Revised December 4, 2017
# Revised April 25, 2018.  Added scanCluster, drawCluster functions, and map_theme object
# Revised July 2018 - placed in tarr package
# Revised April-June 2019 - upgraded makeAddress to strip county and incorporated from city
#         which was causing geocodes to be located in the center of the county.
#         Added simple features functions.
#         Added load_tarrant_spatial for simple feature loading of usual layers.
# Revised May 2021 - geoCases updated for user identified  address fields in a data
#         frame  --------- REMOVED Geocoding functions  ---
# Revised June 2023 - removed sp classes and functions as sp is being removed from CRAN in favor of the sf package
#         removed references to sp and rgeos functions
# Revised July 2023 - refactored the original scanCluster() function to call the newer  scan_cluster().  ScanCluster()
# is no longer generic. Removed the scanPoints() function. 
# Revised April 2025 -- scan_cluster updated to allow selection of DBSCAN, HDBSCAN, and OPTICS. The returned value
# is a cluster object, a sf  descendant  with additional attributes
# ======================================================================================>
# a generic theme for ggplots when creating a map
#' Map theme
#'
#' This theme is based on theme_tufte from ggthemes.  Use this theme for drawing
#' maps with ggplot. Axis labels and ticks are suppressed, and the legend is
#' drawn on the right side.
#'
#' @export
map_theme <- ggthemes::theme_tufte() +
  ggplot2::theme(
    text                = ggplot2::element_text(family = "serif"),
    title               = ggplot2::element_text(face = "bold",size = 10),
    plot.caption        = ggplot2::element_text(hjust = 0,color = "Grey40"),
    strip.text.x        = ggplot2::element_text(hjust = .5),
    #strip.background.x = element_blank(),
    axis.text           = ggplot2::element_blank(),
    axis.title          = ggplot2::element_blank(),
    axis.ticks          = ggplot2::element_blank(),
    strip.text          = ggplot2::element_text(hjust = 0),
    strip.background    = ggplot2::element_rect(fill = NA),
    panel.background    = ggplot2::element_rect(fill = NA),
    panel.grid          = ggplot2::element_blank()
    )


#' Make a complete address
#'
#' Takes address components from a dataframe and makes a complete "cleaned"
#' address. See details for what is meant by "cleaned."
#' Default data frame parameters are based on a line list from an NBS report.
#' However other data frames may be used by identifying the appropriate columns
#' in the dataframe.
#'
#' @details Apartment and suite numbers are stripped when creating the address.
#'   The .city argument is checked for entries that may "fool" a geocoder 
#'   including; "county", "unincorporated", and "un-incorporated". Some geocoders
#'   will place the coordinates at the center of the county and not
#'   use the street portion of the address.  This is prevented by using the
#'   .street, and the .zipcode arguments only when the .city field  is NA.
#'
#' @param .dataframe source dataframe for the following fields
#' @param .street street field, unquoted
#' @param .city address city, unquoted
#' @param .state state field, unquoted
#' @param .zipcode zip code of address, unquoted
#' @return Returns a character vector of formatted addresses
#' @export
makeAddress <- function(.dataframe,
                        .street = Street.Address.1,
                        .city=City, .state = State,
                        .zipcode=Zip.Code){

  stopifnot(is.data.frame(.dataframe))  # check that the 1st argument is a data frame

  # Enquotes the passed names for dplyr referencing.
  # setup passed arguments for refrencing in dplyr call
  .street  <- rlang::enquo(.street)
  .city    <- rlang::enquo(.city)
  .state   <- rlang::enquo(.state)
  .zipcode <- rlang::enquo(.zipcode)

  .dataframe <- .dataframe %>%
    dplyr::select(!!.street, !!.city,!!.state, !!.zipcode) %>%
    dplyr::mutate_if(.predicate = is.factor, .funs = as.character) %>%
    dplyr::mutate(!!.city := correct_city(!!.city)) %>%
    dplyr::mutate(!!.city := ifelse(grepl("(county|coporated)", !!.city,ignore.case = T),"",!!.city))

  #mutate(!!.city = gsub("(county|incorporated)", "", !!.city))

  # Adds commas to make the address more interpretable.
  # Processes the fields, removes apt numbers, trailer numbers, etc
  ret <- mutate(.dataframe,addr = paste0(!!.street, ", ", !!.city, ", ", !!.state, " ",
                         substr(!!.zipcode, start = 1, stop = 5)),
           addr = gsub(pattern = "Trlr", "", addr, ignore.case = T),
           addr = gsub(pattern = "Apt\\.", "", addr, ignore.case = T),
           addr = gsub("#\\s*\\d+", "", addr)) %>%
    dplyr::pull(addr)
  ret <- gsub(pattern = " , ",
              replacement = ", ",
              x = ret) # remove extra blanks
  return(ret) # return the vector of addresses
}

#' Scan for clustered points
#'
#' This function remains in this package for backward compatibility, but is no longer maintained. It may be removed
#' from the package.  Instead use replacement function [scan_cluster()].
#' 
#' @param .points A "[sf::sf]" (simple features).   Tt must have an
#'   assigned CRS. 
#' @param .dt The  name (quoted character) of the date field if present.
#' @param .tf The date range as a lubridate interval to filter records from the dataframe using the .dt field. if NULL
#'   then all dates are used.
#' @param .minPts The minimum number of points (default 3) in in a neighborhood to id clusters. According to the
#'   [dbscan::dbscan()] documentation, .minPts should be approximately 1 + dimensions. Hence x,y (i.e., lat, lon) data
#'   would be 3.
#' @param .eps Distance in CRS units to use for DBSCAN.  The default is 5280, or the number of feet in one mile assuming
#'   a projected CRS using feet is used.
#' @param .crs A character string denoting a projected Coordinate Reference System the function should to use for
#'   transforming .points. The default is to State Planar Feet for North Central Texas (epsg:2276).  See details
#'
#' @details  The Coordinate Reference System passed in .crs argument must be a projected reference system using units
#'   like "feet", "yards",  "meters", et cetera. The .crs argument is used to transform the object in .points and then
#'   distances are calculated.  *Do not* use a geographic CRS in decimal degrees. Do use a projected CRS and one that is
#'   appropriate for the area being studied!  If a .crs does not result in a projected object, an error will be
#'   returned. For more information regarding geographic and projected coordinate reference systems see this [ArcGIS
#'   blog post](https://www.esri.com/arcgis-blog/products/arcgis-pro/mapping/gcs_vs_pcs/).  To find a CRS that may be
#'   useful for your area, refer to this site [Spatial Reference](https://spatialreference.org/).
#'   
#'
#' @return Returns  an sf object of points with added columns including:
#'   \itemize{
#'   \item Cluster: the cluster number.  Non-clustered cases belong to number 0
#'   \item Core: a factor of "Core", "Border", "Not Clustered "
#'   \item Cluster.Title: text of  cluster number, number of cases, and time range cases occurred
#'   }
#'   @keyword internal
#' @export
scanCluster <- function(.points, .dt=NULL, .tf=NULL, .minPts=3, .eps = 5280, .crs = 2276, ...)
{
  if( !missing(.dt))  assert_that(any(class(.points[[ .dt ]]) %in% c("Date", "POSIXct", "POSIXlt")))
  
  # select records in the date interval passed
  if( !missing(.tf) & is.interval(.tf) & !missing(.dt)) .points <- .points[!is.na(.points[[.dt]])  & .points[[.dt]] %within% .tf, ]  
  
  # scan for clusters and rename columns to original names used in this function
  .points <- scan_cluster(points = .points, minPts = .minPts, eps = .eps, crs = .crs) |> 
    rename(Cluster = cluster, Core = point_type)

  #create a description field called Cluster.Title with the cluster number, number of records in the cluster, and time
  #frame for each cluster
  range_format <- compose(
    ~ paste0("(", ., ")"),
    ~ paste(., collapse = " - "),
    ~ format(., format = "%m/%d/%y"),
    ~ range(.x, na.rm =  TRUE)
  )
  
  cls.cnt <- .points %>%
    group_by(Cluster) %>%
    summarise(cases = n(),
              dt_range = ifelse(!is.null(.dt), range_format(.data[[.dt]]), "") 
              ) |> 
    mutate(Cluster.Title = paste0(ifelse(Cluster > 0, LETTERS[Cluster], "-"),
                                  ". ", stringr::str_pad(cases, width = 4, side = "right"),
                                  dt_range),
           Cluster.Title = factor(Cluster.Title)) %>%
    dplyr::select(-dt_range) %>%
    st_set_geometry(NULL)

  .points <- dplyr::left_join(.points,cls.cnt,by = "Cluster")
  rm(cls.cnt)
  return(.points)
}


#' Density Scan for Clusters of Points
#'
#' Identify clusters of point features using density-based clustering algorithms from the `dbscan` package. The
#' `scan_cluster()` function takes an `sf` point object, transforms it to an appropriate projected coordinate reference
#' system (CRS), applies the user-selected clustering algorithm, and then transforms the object back to the original CRS
#' before returning a clustered object. Currently supported algorithms are DBSCAN, HDBSCAN, and OPTICS.
#' 
#' scan_object() extracts the underlying clustering "scan object" from the "cluster" object returned by
#' `scan_cluster()`.
#'
#' scan_unit() retrieves the unit of distance in the "cluster" object returned by `scan_cluster()`.
#' 
#' @details The three density-based clustering algorithms differ mainly in how they handle varying densities and how
#' much structure they reveal. OPTICS is well suited to identifying clusters over a range of densities, HDBSCAN
#' automatically extracts stable clusters from a hierarchical density tree, and DBSCAN is the original, conceptually
#' simpler algorithm that is often easier to understand but less flexible. For an overview of these algorithms, see the
#' `dbscan` package.
#'
#' For additional documentation on DBSCAN, see:
#'
#' - [DBSCAN: density-based clustering for discovering clusters in large datasets with noise - Unsupervised Machine Learning](http://www.sthda.com/english/wiki/wiki.php?id_contents=7940)
#' - [DBSCAN: What is it? When to Use it? How to use it](https://elutins.medium.com/dbscan-what-is-it-when-to-use-it-how-to-use-it-8bd506293818)
#'
#' For OPTICS, see:
#'
#' - [OPTICS: ordering points to identify the clustering structure](https://dl.acm.org/doi/10.1145/304181.304187)
#' - [OPTICS clustering explanation](https://www.geeksforgeeks.org/ml-optics-clustering-explanation/)
#'
#' For an overview of HDBSCAN, see:
#'
#' - [HDBSCAN overview](https://www.geeksforgeeks.org/hdbscan/)
#'
#' All density-based algorithms used here rely on inter-point distances to identify clusters. A projected coordinate
#' reference system (CRS) with linear units (for example, feet, yards, or meters) is therefore required for meaningful
#' distance calculations; using a geographic CRS (for example, degrees of latitude/longitude) will generally produce
#' misleading results. For more background on geographic versus projected CRSs, see:
#'
#' - [ArcGIS blog](https://www.esri.com/arcgis-blog/products/arcgis-pro/mapping/gcs_vs_pcs/)
#' - [Earth Analytics and R](https://www.earthdatascience.org/courses/earth-analytics/spatial-data-r/intro-to-coordinate-reference-systems/)
#' - [ESRI: Using Projection Elements](http://downloads.esri.com/support/documentation/ims_/ArcXML_Guide/Support_files/elements/using_projections.htm)
#'
#' Before clustering, `scan_cluster()` transforms the input `points` from its existing CRS to the projected CRS
#' specified by `crs`, performs the selected density-based clustering in that projected space, and then transforms the
#' clustered object back to the original CRS of `points`.
#'
#' `scan_object()` retrieves the stored *scan object* from a clustered object returned by `scan_cluster()`. This is a
#' thin wrapper around `attr(x, "scan_object")`, and the returned object is whatever was produced by the selected
#' clustering algorithm (for example, a DBSCAN, OPTICS, or HDBSCAN result).
#'
#' `scan_unit()` returns the unit of length used to measure distances between points during clustering. This value
#' corresponds to the units by the projected CRS used inside `scan_cluster()` (for example, meters, feet, or
#' yards), and matches the units associated with the `crs` argument passed to `scan_cluster()`.
#' 
#' @param points An `sf` object of more than five rows with `POINT` geometry and a defined coordinate reference system
#'   (CRS).
#' 
#' @param minPts Minimum number of points required to form a dense neighborhood (cluster).
#'
#' @param eps Distance threshold used to define neighborhoods for DBSCAN and the maximum neighborhood distance for
#'   OPTICS. This argument is ignored by HDBSCAN. The default is 5280 feet (one mile), specified as
#'   `units::set_units(5280, feet)`. If `eps` has a length unit set, its value and unit are converted to match the
#'   length unit of the `crs` argument. If `eps` has no unit, it is interpreted in the units of the projected CRS
#'   specified by `crs`. See `units::set_units()` for setting units (for example, feet, meters, kilometers).
#'
#' @param crs Projected coordinate reference system used for distance calculations. The default,
#'   [`epsg:2276`](https://spatialreference.org/ref/epsg/2276/), is a NAD83 Texas North Central state plane system in US
#'   survey feet, suitable for analyses in and around Tarrant County. The `crs` must be a projected CRS with a
#'   recognized linear unit (for example, feet or meters); geographic CRSs (for example, longitude/latitude in degrees)
#'   are not appropriate for Euclidean distance calculations. When the input `points` object is in a different CRS (such
#'   as EPSG:4326 for geocoded longitude/latitude), it will be transformed to `crs` before clustering. For work outside
#'   North Central Texas, supply a projected CRS that is appropriate for your study area instead of relying on the
#'   default.
#'       
#' @param method Clustering algorithm to use; one of `c("DBSCAN", "OPTICS", "HDBSCAN")`. The default is `"DBSCAN"`.
#'   Note: as of April 2025, the OPTICS option is not working correctly.
#'
#' @param ... Additional arguments passed to the selected clustering function. For example, `cluster_selection_epsilon`
#'   for HDBSCAN can be used to obtain an equivalent DBSCAN clustering at a minimum distance level while still
#'   identifying hierarchical clusters above that level. See the corresponding functions in the `dbscan` package for
#'   supported arguments.
#' 
#' @return `scan_cluster()` returns a clustered `sf` `POINT` object with the same CRS as `points`. Rows with missing
#'   geometries are dropped. Two additional columns are added:
#'
#'   - `cluster`: integer cluster label for each point; `0` indicates noise.
#'   - `point_type`: point classification (`"core"`, `"border"`, or `"noise"`).
#'
#'   The returned object also carries two attributes:
#'
#'   - `"eps_unit"`: character label of the distance unit (for example,
#'   `"foot"`, `"meter"`) used for measuring inter-point distances; use `scan_unit()` to retrieve this.
#'   - `"scan_object"`: the full clustering result object returned by the
#'   selected algorithm; use `scan_object()` to access it. This object includes metadata such as the clustering method,
#'   `eps` (for DBSCAN or for efficiency in OPTICS, and `NA` for HDBSCAN), `minPts`, the number of clusters, and
#'   additional structures such as a hierarchical clustering or dendrogram for HDBSCAN, or a reachability representation
#'   for OPTICS.
#'   
#' @export
#'
#' @examples 
#' library(sf)
#' library(dbscan)
#' # Use fooflu as disease with points
#' data("synthetic_outbreak", package = "tarr")
#' flu <- synthetic_outbreak[synthetic_outbreak$condition == "fooflu", ]
#' 
#'  # convert to a sf points object using a geographic coordinate reference system.
#' flu <- sf::st_as_sf(flu, coords = c("lon", "lat"), crs = "epsg:4326")
#'  
#'  # get DBSCAN clusters using default distance of 5280 feet and default crs for North Central Texas.
#'  flu_clusters <- scan_cluster(method = "DBSCAN", points = flu, minPts = 3, min = "Foo Flu Clusters") 
#'  flu_clusters$cluster |> unique() |> sort()
#'  
#'  # distance unit used 
#'  scan_unit(flu_clusters)
#'  
#'  # scan object information
#'  scan_obj <- scan_object(flu_clusters)
#'  scan_obj
#'  dbscan::ncluster(scan_obj)  # number of clusters
#'  dbscan::nnoise(scan_obj)    # number of noise points
#' 
scan_cluster <- function(points, 
                         minPts = 3, 
                         eps = units::set_units(5280, feet), 
                         crs = "epsg:2276", 
                         method = c("DBSCAN", "OPTICS", "HDBSCAN"), ...) {
  
  dots <- list(...)
  if("xi" %in% names(dots)) xi_arg = dots[["xi"]] %||% 0.07 else xi_arg = 0.07
  
  checkmate::assert_class(points, "sf", .var.name = "points")
  
  checkmate::assert(
    checkmate::check_true(
      all(sf::st_is(sf::st_geometry(points), "POINT"))
    ),
    .var.name = "points",
    .fmt = "All entries in '%s' must be of geometry type 'POINT'."
  )
  
  # points must be sf with POINT geometry and at least 6 rows
  checkmate::assert(
    checkmate::check_true(nrow(points) > 5),
    .var.name = "points",
    .fmt = "Must have more than five points to attempt clustering (got %s rows)."
  )
  
  # points must have a CRS
  checkmate::assert(
    checkmate::check_true(!is.na(sf::st_crs(points))),
    .var.name = "points",
    .fmt = "The '%s' argument must have a CRS."
  )
  
  # eps: single numeric value (with or without units)
  checkmate::assert_numeric(eps, len = 1, any.missing = FALSE, .var.name = "eps")
  
  # minPts: single numeric (integerish) value
  checkmate::assert_numeric(minPts, len = 1, any.missing = FALSE, .var.name = "minPts")
  
  # crs must be projected, not geographic, with linear units
  crs_obj <- sf::st_crs(crs)
  
  checkmate::assert(
    checkmate::check_true(!sf::st_is_longlat(crs_obj)),
    .var.name = "crs",
    .fmt = "The '%s' argument is geographic (degrees); it must be projected (length/distance)."
  )
  
  checkmate::assert(
    checkmate::check_true(!is.null(crs_obj$units)),
    .var.name = "crs",
    .fmt = "The '%s' argument must have a recognized linear unit (e.g., feet or meters)."
  )  
  
  method = match.arg(arg = method, choices = method)
  
  # check that the given crs matches that of the eps if it has any units
  # named vector to get the units from a projected CRS abbreviation, to convert to local units
  abbrev_units <- c("us-ft"          = "feet", # US Survey Foot, but in units is just longer than an international foot
                    "ft"             = "feet", # international foot
                    "mile"           = "mile",
                    "m"              = "meter",
                    "mm"             = "millimeter",
                    "cm"             = "centimenter",
                    "km"             = "kilometer",
                    "yd"             = "yard",
                    "nmi"            = "nautical-mile",
                    "nmi_uk"         = "nautical mile (UK)", #	1,853.184 meters
                    "nmi_us"         = "nautical mile (US)", #	1,853.248 meters"
                    "ch"             = "chain (Gunter's)", #	20.1168 meters
                    "ch_benoit_a"    = "chain (Benoit 1895 A)", #	20.1167824 meters
                    "ch_benoit_b"    = "chain (Benoit 1895 B)", # 20.116782494375872 meters
                    "rd"             = "rod", #	5.0292 meters
                    "rd_us"          = "rod (US)", #	5.0292100584201176 meters
                    "mi"             = "statute mile", #	1,609.344 meters
                    "pt"             = "point", #	0.00035277777777777776 meters, 1/72 of an inch
                    "smoot"          = "smoot", #	1.7018 meters
                    "vara_tx"        = "vara (Texas)", #	0.84666836000338674 meters
                    "yd_benoit_a"    = "yard (Benoit 1895 A)", #	0.9143992 meters
                    "yd_benoit_b"    = "yard (Benoit 1895 B)", #	0.91439920428981236 meters
                    "yd_clarke"      = "yard (Clarke)", #	0.9143917962 meters
                    "yd_indian"      = "yard (Indian)", #	0.91439853074444077 meters
                    "yd_indian_1937" = "yard (Indian 1937)", #	0.91439523 meters
                    "yd_indian_1962" = "yard (Indian 1962)", #	0.9143988 meters
                    "yd_indian_1975" = "yard (Indian 1975)" #	0.9144 meters
  )

    # ensure units  between the points object and desired CRS are the same
   if(inherits(eps, "units")){
     assert_that( attributes(eps)$units$numerator %in% abbrev_units)
     units(eps) <- abbrev_units[st_crs(crs)$units]
   } else{
    eps <- set_units(eps, abbrev_units[st_crs(crs)$units], mode = "standard")
  }
  
  old_crs <- st_crs(points)
  points <- st_transform(points, crs = crs)
  
  # only use points that are not missing/empty
  ret <- points[!sf::st_is_empty(points), ]
  if (nrow(ret) < nrow(points)) warning(nrow(points) - nrow(ret), "Empty POINT objects were removed:" )
  
  point_matrix <- st_coordinates(points)
  
  # run the selected method
  cl <- switch(method,
               "DBSCAN"  = dbscan(point_matrix, eps = as.numeric(eps), minPts = minPts),
               "HDBSCAN" = hdbscan(point_matrix, minPts = minPts, ...),
               "OPTICS"  = {optics(point_matrix, eps = as.numeric(eps), minPts = minPts, ...) |> 
                   dbscan::extractXi(xi = xi_arg)}
               )
  
  #if(any(method  %in% c("DBSCAN", "HDBSCAN"))){
    ret <- augment(cl, points) |> 
      rename(cluster = .cluster) |> 
      mutate(point_type = ifelse(cluster == 0, "noise", "core"))
  #} else {
  #  ret <- dbscan:::augment.general_clustering(cl, points) |> 
  #    rename(cluster = .cluster)
  #}
    
  if( !inherits(ret, what = "sf")) class(ret) <- c("sf", class(ret))
  
  ret <- st_transform(ret, old_crs)
  
  ret <- new_clustered(
    x = ret,
    scan_object = cl,
    eps_unit = units(eps)$numerator
  )
  
  validate_clustered(ret)
  return(ret)
}

# Helper function for scan_cluster
#' Transform points to a projected CRS for clustering
#'
#' @description
#' Internal helper used by `scan_cluster()` to transform an `sf` point object
#' to the projected coordinate reference system (CRS) used for distance-based
#' clustering. The function assumes that basic argument checks (for example,
#' that `points` is an `sf` object with `POINT` geometry and that `crs` is a
#' projected CRS with linear units) have already been performed by the caller.
#'
#' @param points An `sf` object with `POINT` geometry and a valid CRS.
#' @param crs A projected CRS (as accepted by `sf::st_crs()`) to which
#'   `points` will be transformed for clustering.
#'
#' @return An `sf` object with `POINT` geometry transformed to the specified
#'   projected CRS.
#'
#' @keywords internal
transform_points_for_clustering <- function(points, crs) {
  pts_proj <- sf::st_transform(points, crs)
  
  bb <- sf::st_bbox(pts_proj)
  vals <- as.numeric(bb)
  
  if (!all(is.finite(vals))) {
    rlang::abort(
      "Transformation to `crs` produced non-finite coordinates. Check that the CRS of `points` and the supplied `crs` are compatible."
    )
  }
  
  if (!all(abs(vals) < 1e8)) {
    rlang::warn(
      "Projected coordinates are unusually large. Check that the CRS of `points` and the supplied `crs` are appropriate for the analysis."
    )
  }
  
  pts_proj
}

# function to retrieve scan_object from a clustered object
#' @param x clustered object returned from the scan_cluster() function.
#' 
#' @returns 
#'  The scan_object() returns the scan_object produced by the selected method in the scan_cluster() function. 
#'  The scan_unit() function returns the unit like "ft", "mile", etc. used for distances in 
#'  
#' @rdname scan_cluster
#' @export
scan_object <- purrr::attr_getter("scan_object")

#' @rdname scan_cluster
#' @export
scan_unit <- purrr::attr_getter("eps_unit")
scan_unit <- purrr::attr_getter("eps_unit")



#' Plot clustered and non-clustered cases on a map
#'
#' Maps identified clusters from either a "SpatialPointsDataframe" or "sf" object with identified clusters in a column.
#' Returns a ggplot as a map that may be used directly or for further customization.
#'
#' The clusters should be previously identified which is may be done with
#' the [scanCluster()] function.
#'
#' @param .bkgrnd background map as a ggplot. The background  should come from  [ggmap::get_map()] that should cover
#'   the area where cases are located.
#' @param .points an "sf" object with the  cases and  a .cluster field and .core field.  The [scanCluster()] function
#'   adds compatible columns to the sf object when called.  
#' @param .cluster column name that identifies the cluster, e.g., Cluster, or Cluster.Title.
#' @param .core column name that designates the 'Core', 'Border', or 'Not Clustered' for each record. 
#' @param .tf is the lubridate date interval covered by .points and is used in the subtitle of the chart.
#' @param .cond condition as a character string used in the title. If it is  NULL, no condition is shown.
#' @param .drawCluster determines if convex polygons (default TRUE) that surround a cluster are used on the map.
#'
#' @usage
#' mapCluster(.points, .bkgrnd, .cluster, .core, .tf = NULL,
#' .cond = NULL, .drawCluster = TRUE)
#'
#' mapCluster.sf(.points, .bkgrnd, .cluster, .core, .tf = NULL,
#' .cond = NULL, .drawCluster = TRUE)
#'
#' @return a ggplot map with clustered cases.
#' @keywords internal
#' @export
mapCluster <- function(.points, .bkgrnd, .cluster, .core, .tf = NULL,.cond = NULL, .drawCluster = TRUE) UseMethod("mapCluster")


#' @export
mapCluster.sf <- function(.points, .bkgrnd, .cluster, .core, .tf = NULL,.cond = NULL, .drawCluster = TRUE) {
  .cluster <- rlang::enquo(.cluster)
  .core    <- rlang::enquo(.core)
  
  stopifnot(ggplot2::is.ggplot(.bkgrnd))
  
  # create the title for the chart
  tle <- "Geographically Clustered Households"
  if(!is.null(.cond)) tle <- paste(tle,"with", .cond)
  
  if(!is.null(.tf)){
    bgn <- format(int_start(.tf), format="%b %d, %Y")
    lst <- format(int_end(.tf), format="%b %d, %Y")
    tle <- paste0(tle,"\n",
                  bgn, " - ", lst)
    rm(bgn,lst)
  }
  
  cluster_count <- .points |> 
    pull( {{.cluster}} ) |> 
    unique() |> 
    length()
  
  pal_name <- if(cluster_count < 9) "Dark2" else if(cluster_count < 13) "Paired" else { 
    stop("Clusters: ", cluster_count,  "Must be 12 or less to assign colors")
  }
  
  cases.pal <- RColorBrewer::brewer.pal(n = cluster_count, name = pal_name)
  rm(pal_name)
  
  plot <- .bkgrnd +
    ggplot2::geom_sf(data = .points,
                     mapping = ggplot2::aes(
                       color = factor({{ .cluster }}),
                       shape = {{ .core }}
                     ),
                     size=3, inherit.aes = FALSE ) +
    ggplot2::scale_color_manual(values = c("Grey60", cases.pal), 
                                name   = as_name(.cluster)) +
    ggplot2::scale_shape_manual(values = c("Core" = 19,"Border" = 21,"Not Clustered" = 13),
                                name   = "Core or Border",
                                guide  = ggplot2::guide_legend(reverse = TRUE)) +
    ggplot2::labs(title   = tle,
                  caption = format(citation("dbscan"),style="text")) +
    map_theme
  
  # Draw polygons around clusters
  if(.drawCluster) {
    # do not get a polygon for the non-clustered points which should be the first listed cluster
    clusters   <- .points[[as_name(.cluster)]] |> unique() |> sort() # unique cluster identifiers
    cluster_id <- clusters[2:(length(clusters))]  # assumes first cluster id is non-clustered cases
    include_recs <- .points[[as_name(.cluster)]] %in% cluster_id
    points_for_polys <- .points[include_recs,]
    polys <- get_cluster_polys(points_for_polys, UQ(.cluster), dist = 200)
    plot <- plot+
      ggplot2::geom_sf(data = polys, 
                      mapping = ggplot2::aes( fill = factor({{ .cluster }})), 
                      alpha = .4, 
                      inherit.aes = FALSE) +
      ggplot2::scale_fill_manual(values = cases.pal, guide = "none")
    rm(polys)
  }

  return(plot)
}

#---- get_cluster_polys is a function that makes convex polygons for clusters
#' Create polygons around  clusters
#'
#' Creates polygons based on cluster identifiers in an sf object.  Each polygon is the convex hull of a set of clustered
#' points. Polygons are created for all identified clusters in the points argument.  
#' 
#' @param points is the simple features with points geometry
#' @param cluster is the unquoted column name that identifies clusters. This can be an integer, numeric or character
#'   vector.
#' @param dist is the distance to increase the size (buffer)the returned polygons using the same units as found in
#'   points.
#' @param ... arguments passed to the get_poly() function
#'
#' @return an sf object  with one record for each polygon around each identified cluster of points. Returns NULL if
#' points is empty.
#' @export
#'
#' @seealso [get_poly()] for creating a polygon around passed points and [mapCluster()] for mapping clusters with
#'   ggplot.
#' @examples
#' 
#' foo <- synthetic_outbreak[synthetic_outbreak$condition == "fooflu", ] |>
#'   st_as_sf(coords = c("lon", "lat"))
#' st_crs(foo) <- "epsg:4326"
#' foo <- st_transform(foo, "epsg:2276")
#' 
#' # dbscan for clusters
#' foo_dbscan <- scan_cluster(points = foo, eps = 5280*2.5, minPts = 6)
#' 
#' polys <- foo_dbscan |> get_cluster_polys(cluster = cluster, ratio = 0.7)
#' plot(st_geometry(foo_dbscan), col = "red")
#' plot(poly_1, add= TRUE, col = NA, border = "blue")
get_cluster_polys <- function(points, cluster, dist = 0, ...) {
  assertthat::assert_that("sf" %in% class(points))
  if(nrow(points) == 0) return(NULL)
    #clusters   <- points |> pull( {{cluster}} ) |> unique() |> sort() # unique cluster identifiers
    #cluster_id <- clusters[1:(length(clusters))]  # assumes first cluster id is non-clustered cases
    
    polys <- points |> 
     # filter( {{cluster}} %in% cluster_id) |> 
      group_by( {{cluster}}) |> 
      summarise( geometry = get_poly(geometry, dist, ...) ) 

    return(polys)
}

#' Polygon from a set of points
#'
#'  Computes the convex hull of a set of points in an sf object.  
#'  
#' @param points a set of records such as from an sf or sfc as a point geometry.
#' @param dist is the distance to buffer the returned polygon.
#'
#' @return a POLYGON object
get_poly <- function(points, dist = 0, ...) {
  args <- list(...)
  dots <- list(...)
  if("ratio" %in% dots) ratio = dots[["ratio"]] else ratio = .50  # default
  if(length(args)){
    if("ratio" %in% names(args)) ratio <- args[["ratio"]]
    
  }
  st_combine(points) |> 
    sf::st_concave_hull(ratio = ratio, allow_holes = FALSE) |> 
    #concaveman::concaveman() |> 
    #st_convex_hull() |> 
    st_buffer(dist)
}




#' Load the named spatial data set
#'
#' By passing the name of a spatial data set (see \code{\link{geo_sets}} for the
#' names) the function will load it and optionally change the coordinate
#' reference system (CRS). The alternative to this function is to use
#' geo_sets$name to retrieve the path to the file that holds the spatial data
#' set, then load it with readRDS.  Note that all of the data sets are stored
#' with CRS = 4326 which is a geographic CRS and is the same used by Google
#' Maps, Bing, etc.
#' 
#' @param .name of the spatial data set to return
#' @param .crs (optional) the coordinate reference system to transform the
#'   spatial data set. Default is EPSG:4326
#'
#' @return The requested spatial data set as a simple feature object.  if .crs
#'   is not specified, then the object CRS is EPSG:4326, the same as
#'   tiles returned by Google maps.
#'
#' @export
load_tarrant_spatial <- function(.name = c("ZCTA", "tarrant_border", "tarrant_tracts",
                                 "surrounding_borders", "tarrant_blocks", "tarrant_cities",
                                 "isd"), .crs = 4326){
  stopifnot(!missing(.name) | is.character(.name))
  stopifnot(.name %in% names(geo_sets))
  ret <- readRDS(file = geo_sets[[.name]])
  if(!is.null(.crs)){
     ret <- st_transform(ret, .crs)
  }
  return(ret)
}


#' Standardize Tarrant City Names
#'
#' Checks and corrects mis-spelling of Tarrant County cities using exact and fuzzy matching. Within the function, the
#' case of the city is standardized to title case. Text after a comma is removed (e.g. ', Tx'). Common abbreviations are
#' expanded (e.g. Ft -> Fort, N. -> North). The fuzzy matching is accomplished using the [stringdist::amatch()] function
#' with all cities found in [city.names], and [city.names.outside].
#'
#' @param .city a character vector of city names
#' @param .small is the number (and smaller) of characters for small cities (defaults to 5).
#' @param .thresholds a numeric vector of size two for thresholds to use for the distance for small and large city
#'   names. Default is c(0.18, 0.20).
#' @param .method is the string dissimilarity measure to use. The default is "jw".
#' @param .addtional_cities can be used to correct other city names that are not in [city.names.outside]
#'
#' @details The function's use of [stringdist::amatch()] results in a vector of best matches or NA for those matches
#'   above the threshold. The Jaro-Winkler distance method is used by default. Other available methods are found at
#'   [stringdist::stringdist-metrics]. Two different distance thresholds can be specified one for small city names
#'   (e.g., 5 or fewer characters) and another for larger city names (e.g. more than 5 characters). Generally a dist of
#'   0.18 is good for names with 5 or fewer characters and 0.20 for names with more than 5 characters.
#' 
#' @return A character vector with corrected names spelling in title case
#' @export
correct_city <- function(.city, .small = 5, .thresholds = c(0.18, 0.2), .method = "jw", .addtional_cities = NULL) {
  
  if(!is.null(.addtional_cities)){
     .addtional_cities <- stringr::str_to_title(.addtional_cities)
  }
  
  city_proc <- .city %>%
    # standardize the city names to title case
    stringr::str_to_title() %>%
    # remove trailing comma and anything after that, e.g. ", Tx"
    stringr::str_extract( "^[^,]*")  %>%
    stringr::str_remove("(Texas|Tx)") %>%
    gsub("Other ?-?", "", .) %>% 
    stringr::str_trim(side = "both")
  
  # Expand or replace common abbreviations, use title case
  acryn <- c("^No?\\.? "         = "North ",
             "^Ft\\.? "          = "Fort ",
             "^Forth"            = "Fort ",
             "^Ftw"              = "Fort Worth",
             "^Fw$"              = "Fort Worth",
             "Fortworth"         = "Fort Worth",
             "^Arl$"             = "Arlington",
             "^N.?r.?h$"         = "North Richland Hills",
             "^North ?Richland"  = "North Richland Hills",
             "Richland H\\.?i?$" = "Richland Hills",
             " Grdns?$"          = " Gardens",
             " Hls$"             = " Hills",
             " Vlg$"             = " Village",
             "^Alito$"           = "Aledo",
             " ?Jrb ?"           = "Joint Reserve Base ",
             "^Nas "             = "Naval Air Station "
  )
  
  city_proc <- city_proc %>%
    stringr::str_replace_all(pattern = acryn)
  rm(acryn)
  
  # remove words or letters in the beginning that confuse the fuzzy differences
  # especially for Azle
  removals <- c("(^Arli$|^Arly$|^At$|^Bed$|^Drive$|^Gv$|^Neh$|^Rd$|^Apt ?)")
  city_proc <- city_proc %>% 
    {stringr::str_remove_all(., pattern = stringr::str_to_title(removals)) %>% 
        stringr::str_remove_all(., pattern = "[:punct:]")}
  rm(removals)
  
  # cities to search are from these character vectors
  all_cities <- c(tarr::city.names, tarr::city.names.outside, .addtional_cities)
  
  # work with only those city names that don't already have match in the city.names 
  # vector.  Save the index of unmatched records to save back to city_proc later
  city_unmtached_ndx <- which(! city_proc %in% all_cities)
  city_unmatched <- city_proc[city_unmtached_ndx]
  
  # cities are matched to comparison cities in two steps, cities spelled with
  # .small or fewer characters and those with more than .small.  The change
  # between the two is the maxdist allowed in the amatch function.
  
  # Small city names first.  
  small_ndx    <- which(x = (nchar(city_unmatched) <= .small))
  compare_city <- all_cities[nchar(all_cities) <= .small]
  matched_ndx  <- stringdist::amatch(x = city_unmatched[small_ndx], 
                                    table = compare_city, 
                                    method = .method, maxDist = .thresholds[1])
  
  city_unmatched[small_ndx] <- ifelse(is.na(matched_ndx), 
                                      city_unmatched[small_ndx], 
                                      compare_city[matched_ndx])
  
  rm(small_ndx, compare_city, matched_ndx)
  
  # Large city names 
  large_ndx <- which(x = (nchar(city_unmatched) > .small))
  compare_city <- all_cities[nchar(all_cities)  > .small]
  matched_ndx <- stringdist::amatch(x = city_unmatched[large_ndx], 
                                    table = compare_city, 
                                    method = .method, maxDist = .thresholds[2])
  
  city_unmatched[large_ndx] <- ifelse(is.na(matched_ndx), 
                                      city_unmatched[large_ndx], 
                                      compare_city[matched_ndx])
  rm(large_ndx, compare_city, matched_ndx)
  
  # save the newly matched cities back to those unmatched in city_proc
  city_proc[city_unmtached_ndx] <- city_unmatched
  
  return(city_proc)
}

