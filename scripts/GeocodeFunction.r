# ======================================================================================
# GeocodeFunction.r
# Functions to take a data frame and geocode it.
# These functions have defaults that work with the "OneDisease Cluster.r: script
# However they are generalized to work other dataframes by naming the needed columns
# Functions:
# makeAddress - takes fields and returns a vector of formatted addresses
# geoCases - takes two dataframes with addressees, ID , and lat lon fields
# compares them and geocodes those that are missing
# scanCluster - uses DBSCAN to ID cluster in a SpatialPointsDataframe
# mapCluster takes the idenitifed clusters from scanCluster and maps them
#
# mapTheme object is a preset them for maps, based on ggthemes:tufte
# 

# These function are  called from "OneDisease cluster.r"
# and the defaults are based on that script which in turn is based
# on the NBS line list with jurisdiction security
#
# By R Jones
# Created August 1, 2017
# Revised December 4, 2017
# Revised April 25, 2018.  Added scanCluster, drawCluster functions, and map_theme object
# Revised July 2018 - placed in tarr package#
# Revised April-June 2019 - upgraded makeAddress to strip county and incorporated from city
#         which was causing geocides to located in the center of the county.
#         Added simple features functions.
#         Added load_tarrant_spatial for simple feature loading of usual layers.
# Revised May 2021 - geoCases updated for user identified  address fields in a data
#         frame
# --------- REMOVED Geocoding functions  ---
# ======================================================================================
# a generic theme for ggplots when creating a map
#' Map theme
#'
#' This theme is based on theme_tufte from ggthemes.  Use this theme for drawing
#' maps with ggplot. Axis labels and ticks are suppressed, and the legend is
#' drawn to the right side.
#'
#' @export
map_theme <- ggthemes::theme_tufte() +
  ggplot2::theme(
    title = ggplot2::element_text(face = "bold",size = 10),
    plot.caption = ggplot2::element_text(hjust = 0,color = "Grey40"),
    strip.text.x = ggplot2::element_text(hjust = .5),
    #strip.background.x = element_blank(),
    axis.text = ggplot2::element_blank(),
    axis.title = ggplot2::element_blank(),
    axis.ticks = ggplot2::element_blank(),
    strip.text = ggplot2::element_text(hjust = 0),
    strip.background = ggplot2::element_rect(fill = NA),
    panel.background = ggplot2::element_rect(fill = NA),
    panel.grid = ggplot2::element_blank())


#' Make a complete address
#'
#' Takes address components from a dataframe and makes a complete "cleaned"
#' address. See details for what is meant by "cleaned."
#' Default data frame parameters are based on a line list from an NBS report.
#' However other data frames may be used by identifying the appropriate columns
#' in the dataframe.
#'
#' @details Apartment and suite numbers are stripped when creating the address.
#'   The .city argument is checked for entries that may "fool" the geocoder
#'   including; "county", "unincorporated", and "un-incorporated". A geocoder
#'   like Google will place the coordinates at the center of the county and not
#'   use the street portion of the address.  This is prevented by using the
#'   .street, and the .zipcode arguments only when the .city field does not
#'   contain an actual city.
#'
#' @param .dataframe source dataframe for the following fields
#' @param .street street field
#' @param .city address city
#' @param .state state field
#' @param .zipcode zipcode of address
#' @return Returns vector of formatted addresses
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
  # Proccesses the fields, removes apt numbers, trailer numbers, etc
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

# Geocode addresses of cases
#
# Geocodes addresses using the Google API via the ggmap packagae.  The user must
# have a Google API key with the geocoding API enabled.
# Takes a dataframe(s) with an id field(s) and addresses to geocode. If two
# data frames are passed, the second dataframe is assumed to contain previously
# geocoded records identified by an ID field. The ID fields of each data frame are
# compared and only the missing records from .dataframe are processed.
# This function requires separate address fields and the default names are those 
# taken from an NBS report.  
#
#
# @param .dataframe contains the addresses to geocode and the unique ID
# @param .id  the ID field in .dataframe, default is Investigation.ID
# @param .street unquoted street field name
# @param .city unquoted  city field name
# @param .state unquoted state field name
# @param .zipcode unquoted zipcode field name
# @param .prev.dataframe optional, contains those IDs that will not be geocoded
# @param .id_prev unquoted ID field name in .prev_dataframe.  This is required when
#   .prev.dataframe is used
# @return Returns a dataframe with the .id field, lon, and lat.  Coordinates using
#   crs = 4326 
# @export
geoCases <- function(.dataframe,
                     .id = Investigation.ID,
                     .street = Street.Address.1,
                     .city=City, .state = State,
                     .zipcode=Zip.Code,
                     .prev_dataframe=NULL,
                     .id_prev
){
  stopifnot(is.data.frame(.dataframe))
  id      = rlang::enquo(.id)
  street  = rlang::enquo(.street)
  city    = rlang::enquo(.city)
  state   = rlang::enquo(.state)
  zipcode = rlang::enquo(.zipcode)
  
  if(!is.null(.prev_dataframe)){
    id_prev = rlang::enquo(.id_prev)
  }
  
  df <- .dataframe %>% 
    select(!! id, !! street, !! city, !! state, !! zipcode)
  
  #stopifnot(is.character(.id))
  #stopifnot(any(grepl(.id,names(.dataframe))))
  
  # geocode addresses that do not exist in the older data frame
  if(!is.null(.prev_dataframe)){
    cases.new <- df[!(df %>% pull(!! id) %in% .prev_dataframe %>% pull(!! id_prev))]
  } else {
    cases.new <- df   # Geocode all cases in the .dataframe
  }
  if (nrow(cases.new) > 0) {
    addr <- makeAddress(cases.new) # Prepare addresses for geocoding
    latlon <- ggmap::geocode(location = addr, output = "latlona", source = "google") # geocode
    ret    <- dplyr::bind_cols(cases.new[, 1], latlon[, 1:2])  # add lat lon to cases.new data frame
    rm(latlon)
  } else {
    ret <- data.frame(.id = character(0),lon=numeric(0),lat=numeric(0))   # no changes done so returns an empty data frame
  }
  names(ret)[1] <-  rlang::as_label(id)
  return(ret)
}




#' Scan for clustered points
#'
#' Identifies clusters based on DBSCAN.  A date interval as defined in the
#' lubridate package is used to select cases to examine for clustering, if a
#' date interval/range is not passed, all records in .points are used.
#'
#' @param .points is either a "SpatialPointsDataframe" or "sf" object with the
#'   cases.  It must have an assigned CRS.
#' @param .dt the  name of the date field if present.
#' @param .tf the date range as a lubridate interval to select from the
#'   dataframe using the .dt field. if NULL then all dates are used.
#' @param .minPts the minimum number of points (default 3) in a cluster to pass
#'   to DBSCAN.  According to DBSCAN documentation, .minPts should be
#'   approximately 1 + dimensions. Hence x,y (i.e., lat, lon) data would be 3.
#' @param .eps distance in CRS units to use for DBSCAN.  The default is 5280, or
#'   the number of feet in one mile.
#' @param .crs Coordinate Reference System to use as a character string,
#'   defaults to State Planar Feet for North Central Texas (epsg:2276).  When
#'   passing a CRS, the scan function needs units from a projected CRS, like
#'   feet or meters.  Do not use a geographic CRS in decimal degrees.Do use a
#'   projected CRS and one  that is appropriate for the area being studied!  If
#'   .crs does not result in a projected object, an error will be returned.
#'
#' @return Returns the spatial object i.e., a SpatialPointsDataframe, or a
#'   Simple feature of points. Added columns  in the retuned object include:
#'   \itemize{
#'   \item Cluster: the cluster number.  Non-clustered cases belong to number 0
#'   \item Core: a factor of "Core", "Border", "Not Clustered "
#'   \item Cluster.Title: text of  cluster number, number of cases, and time range cases occurred
#'   }
#'   Will throw an error when there are no points to scan due to conditions passed
#' @export
scanCluster <- function(.points, .dt=NULL, .tf=NULL, .minPts=3, .eps = 5280, .crs = "+init=epsg:2276") UseMethod("scanCluster")

#' @export
scanCluster.SpatialPointsDataFrame <- function(.points, .dt=NULL, .tf=NULL, .minPts=3, .eps = 5280, .crs = "+init=epsg:2276")
{
  # check argument types
  stopifnot(class(.points) == "SpatialPointsDataFrame" | nrow(.points@data) > 0)
  stopifnot(class(.tf) == "Interval" | is.null(.tf))
  stopifnot(is.numeric(.eps))
  stopifnot(is.numeric(.minPts))
  .minPts <-  as.integer(.minPts)
  date_col <- rlang::enquo(.dt)

  # .points is the SpatialPointsDataframe passed into the function and
  # used for the rest of the function  then returned
  oldPrj <- sp::proj4string(.points)
  .points <- spTransform(.points,CRSobj = CRS(.crs)) # use the passed CRS, something in feet or meters

  stopifnot(sp::is.projected(.points))  # do not proceed if .points is not in a projected CRS.

  # if date_col and .tf are not null, limit the points to scan to the passed time frame
  if(!is.null(date_col) & !is.null(.tf)){
      stopifnot(any(class(rlang::eval_tidy(data = .points@data,date_col)) %in% c("Date", "POSIXct", "POSIXlt")))
      tmp <- cbind(.points@data %>% dplyr::select(!!date_col),TMP_ID = seq_len(nrow(.points@data))) %>%
        filter((!!date_col) %within% .tf)   # get rows with the included dates
      .points <- .points[tmp$TMP_ID,]
      rm(tmp)
      if(nrow(.points@data) == 0){
        stop("Passed date conditions created a condition of no points to scan")
      }
    }
  # else {
  #     .date_col = NULL
  #   }

  stopifnot(nrow(.points@data) > 0)  # prevent fatal error, must have actual points to scan
  cluster_pts <- scanPoints(.df = sp::coordinates(.points), .eps = .eps, .minPts = .minPts)

  # add lat lon and cluster information to .points, ensure that duplicate names are not added
  # since .points is a SpatialPointsDataFrame, do not re-add the lat and lon
  .points@data[fields <- names(.points@data) %in% names(cluster_pts)] <- NULL # remove possible duplicate fields
  .points@data <- cbind(.points@data, cluster_pts[,!(names(cluster_pts) %in% c("lat","lon"))])  # add cluster information including lat lon

  # create a description field with cluster number,
  # of records in cluster, and time frame for each cluster
  # if date_col is not null, add date ranges to the cluster title
  if(!is.null(date_col)){
     cls.cnt <- .points@data %>%
     group_by(Cluster) %>%
     summarise(Cases = n(),
               First = ifelse(!is.null(!!date_col), min(!!date_col, na.rm=T),""),
               Last  = ifelse(!is.null(!!date_col), max(!!date_col, na.rm=T),"")) %>%
     mutate(   First = as.Date(First, origin = "1970-01-01"),
               Last  = as.Date(Last,  origin = "1970-01-01"),
       Cluster.Title = paste0(ifelse(Cluster > 0, LETTERS[Cluster], "-"),
                                   ". ",stringr::str_pad(Cases, width = 4, side = "right"),
                                   paste0(" (",format.Date(First,"%m/%y"),"-",format.Date(Last,"%m/%y"),")"))
            )

 }
  else {
  cls.cnt <- .points@data %>%
    group_by(Cluster) %>%
    summarise(Cases = n()) %>%
    mutate(Cluster.Title = paste0(ifelse(Cluster > 0, LETTERS[Cluster], "-"),
                                  ". ",stringr::str_pad(Cases, width = 4, side = "right")))
  }

  #   addition <- .points@data %>%
  #     group_by(Cluster) %>%
  #     summarise(Cases = n()) %>%
  #             paran = paste0("(",
  #                        format(as.Date(First, origin="1970-01-01"), format = "%m/%d/%y")," - ",
  #                        format(as.Date(Last,origin="1970-01-01"), format = "%m/%d/%y"), ")")) %>%
  #       dplyr::select(paran, Cluster)
  #   print(paste("Rows in addition:", nrow(addition)))
  #   print(paste("rows in cls.cnt:", nrow(cls.cnt)))
  #
  #   # cls.cnt <- cbind(cls.cnt, addition) %>%
  #   #   mutate(Cluster.Title = paste0(Cluster.Title,paran))
  #   rm(addition)
  #  }

  .points@data <- left_join(.points@data,cls.cnt,by = "Cluster")
  rm(cls.cnt)

  .points <- sp::spTransform(.points,CRSobj = CRS(oldPrj))  # return to original CRS
  return(.points)
}


# scanCluster.sf is used for simple feature point objects
#' @export
scanCluster.sf <- function(.points, .dt = NULL, .tf = NULL, .minPts = 3, .eps = 5280, .crs = 2276, .method = "DBSCAN") {
  # check argument types
  assert_that(inherits(.points, "sf"))
  assert_that(!st_is_empty(.points))
  assert_that( all(st_is(st_geometry(.points, "POINT"))))
  assert_that(class(.tf) == "Interval" | is.null(.tf))
  assert_that( any(class(rlang::eval_tidy(data = .points, date_col)) %in% c("Date", "POSIXct", "POSIXlt")) )
  assert_that(is.numeric(.eps))
  assert_that(is.numeric(.minPts))
  .minPts <- as.integer(.minPts)
  date_col <- rlang::enquo(.dt)

  # Use a projected CRS for clustering analysis.  A Geographic CRS will cause errors in the clustering algorithms
  oldPrj <- sf::st_crs(.points)
  .points <- sf::st_transform(x = .points, crs = .crs) # use crs passed into function

  # do not proceed if .points is not in a projected CRS.
  # checks for long lat coordinates as an error.  No direct  replacement for
  # sp::is.projected()
  stopifnot(!sf::st_is_longlat(.points))
  # tmp <- cbind(.points %>% dplyr::select(Event.Date),TMP_ID = seq_len(nrow(.points))) %>%
  #   filter(Event.Date %within% .tf)   # get rows with the included dates
  # .points <- .points[tmp$TMP_ID,]
  #

  if (!is.null(.tf)) { # select records in the date interval passed
    tmp <- cbind(.points %>% dplyr::select(!!date_col), TMP_ID = seq_len(nrow(.points))) %>%
      filter((!!date_col) %within% .tf) # get rows with the included dates
    .points <- .points[tmp$TMP_ID, ]
    rm(tmp)
  }

  # find clustered points and add to .points
  cluster_pts <- scanPoints(.df = as.data.frame(sf::st_coordinates(.points)), .eps = .eps, .minPts = .minPts)
  .points <- st_sf(cbind(as.data.frame(.points), cluster_pts))


  # create a description field with cluster number,
  # of records in cluster, and time frame for each cluster
  cls.cnt <- .points %>%
    group_by(Cluster) %>%
    summarise(
      Cases = n(),
      First = ifelse(!is.null(!!date_col), min(!!date_col, na.rm = T), ""),
      Last = ifelse(!is.null(!!date_col), max(!!date_col, na.rm = T), "")
    ) %>%
    mutate(Cluster.Title = paste0(
      ifelse(Cluster > 0, LETTERS[Cluster], "-"),
      ". ", stringr::str_pad(Cases, width = 4, side = "right"),
      "(",
      format(as.Date(First, origin = "1970-01-01"), format = "%m/%d/%y"), " - ",
      format(as.Date(Last, origin = "1970-01-01"), format = "%m/%d/%y"), ")"
    )) %>%
    dplyr::select(-First, -Last) %>%
    st_set_geometry(NULL)


  .points <- dplyr::left_join(.points, cls.cnt, by = "Cluster")
  rm(cls.cnt)
  .points <- sf::st_transform(x = .points, crs = oldPrj) # return to original CRS
  return(.points)
}


# Internal function to scan points coordinates for clusters using DBScan This
# function is called from the scanCluster function
# It makes no attempt to change the CRS. Therefore it is  up
# to the calling functin to adjust and interpret the appropriate CRS
#
# @param .df is the longtitude and latitude coordinates to scan as a dataframe
#   or matrix.  The first column should be the latitude (Y), the second column
#   should be longitude (X).
# @param .eps is the distance to use for clustering
# @param .minpts is the mimimum number of points to use in DBScan
#
# @return a dataframe (.df) with added columns for
# \itemize{
#  \item Cluster: identifies the cluster number the point it blongs, 0 means non-clustered
#  \item Core: if in a cluster is the point on the biorder or a core point?
#  }
#
scanPoints <- function(.df, .eps, .minPts) {
  stopifnot(!is.null(.df) | is.matrix(.df))
  sc.all <- dbscan::dbscan(x = .df,eps = .eps, minPts = .minPts, borderPoints = T)
  sc.core <-  dbscan::dbscan(x = .df, eps = .eps,minPts = .minPts, borderPoints = F)
  .df <- as.data.frame(.df)   # prevents warning about coerciing .df to a list
  .df$Cluster <- sc.all$cluster
  .df$Core <- ifelse(sc.core$cluster > 0, "Core",
                     ifelse(.df$Cluster > 0, "Border", NA))
  .df$Core[.df$Cluster == 0] <- "Not Clustered"
  .df$Core <- factor(.df$Core)
  return(.df)
}


#' Plot clustered and non-clustered cases on a map
#'
#' Maps identified clusters from either a "SpatialPointsDataframe" or "sf"
#' object with identified clusters in a column. Returns a ggplot
#' as a map that may be used directly or for further customization.
#'
#' The clusters should be previously identifed which is may be done with
#' the scanCluster function.
#'
#' @param .bkgrnd background map from ggmap that should cover the area where
#'   cases occur.
#' @param .points either a "SpatialPointsDataframe" or "sf" object with the
#'   cases and  a .cluster field and .core field
#' @param .cluster field that identifies the cluster, e.g., Cluster.Title
#' @param .core name of the field that designates core or border
#' @param .tf is the lubridate date interval covered by .points
#' @param .cond condition as a character string, if NULL the condition will not
#'   be in the map title
#  @param .dt name of the date field in .points used to get the range of dates
#   for the map title
#' @param .drawCluster determines if convex polygons (default TRUE) that
#'   surround a cluster are used on the map
#'
#' @usage
#' mapCluster(.points, .bkgrnd, .cluster, .core, .tf = NULL,
#' .cond = NULL, .drawCluster = TRUE)
#'
#' mapCluster.SpatialPointsDataframe(.points, .bkgrnd, .cluster, .core, .tf = NULL,
#' .cond = NULL, .drawCluster = TRUE)
#'
#' mapCluster.sf(.points, .bkgrnd, .cluster, .core, .tf = NULL,
#' .cond = NULL, .drawCluster = TRUE)
#'
#' @return a ggplot map with clustered cases.
#' @export
mapCluster <- function(.points, .bkgrnd, .cluster, .core, .tf = NULL,.cond = NULL, .drawCluster = TRUE) UseMethod("mapCluster")

#' @export
mapCluster.SpatialPointsDataFrame <- function(.points, .bkgrnd, .cluster, .core, .tf = NULL,.cond = NULL, .drawCluster = TRUE) {

  .cluster <- rlang::enquo(.cluster)
  .core    <- rlang::enquo(.core)
  stopifnot(class(.points) == "SpatialPointsDataFrame")
  stopifnot(is.ggplot(.bkgrnd))

  tle <- "Geographically Clustered Households"
  if(!is.null(.cond))
  {
    tle <- paste(tle,"with", .cond)
  }

  if(!is.null(.tf)){
    bgn <- format(int_start(.tf), format="%b %d, %Y")
    lst <- format(int_end(.tf), format="%b %d, %Y")
    tle <- paste0(tle,"\n",
                  bgn, " - ", lst)
    rm(bgn,lst)
  }

  cases.pal <- RColorBrewer::brewer.pal(n = 8, name = "Dark2")

  # prepare coordinates for use in ggplot
  pts <- data.frame(coordinates(.points))
  names(pts) <- c("lon_tmp","lat_tmp")  # given tmp as name extensions to prevent duplicate columns in the data frame
  .points@data <- cbind(pts,.points@data)
  plot <- .bkgrnd +
    geom_point(data=.points@data, aes_string(x="lon_tmp", y = "lat_tmp", color=quo_name(.cluster),
                                             shape=quo_name(.core)), size=3)+
    scale_color_manual(values = c("Grey60", cases.pal), #guide = FALSE,
                       name = "Cluster. Cases (Date Range)") +
    scale_shape_manual(values = c("Core"=19,"Border"=21,"Not Clustered"=18),name = "Core or Border",guide = guide_legend(reverse = TRUE)) +
    labs(title = tle,
         caption = format(citation("dbscan"),style="text")) +
    map_theme

  # needs completing, tests calling drawCluster
  if(.drawCluster) {
    polys <- getPolys(.points, UQ(.cluster), .dis = 1500)
    plot <- plot+
      geom_polygon(data=polys,aes(x=long,y=lat,group=group,
                                         fill=Cluster),alpha=.4)+
      scale_fill_manual(values = cases.pal, guide=FALSE)
    rm(polys)
  }

  return(plot)
}

#' @export
mapCluster.sf <- function(.points, .bkgrnd, .cluster, .core, .tf = NULL,.cond = NULL, .drawCluster = TRUE) {
  cluster_col <- rlang::enquo(.cluster)
  core_col    <- rlang::enquo(.core)

  # convert the sf object to SpatialPointsDataFrame and pass to the other mapCluster function
  .points <- .points %>%
    dplyr::select(Cluster = !!cluster_col,
           Core = !!core_col,
           geometry) %>%
    as('Spatial')

  mapCluster.SpatialPointsDataFrame(.points, .bkgrnd, .cluster = Cluster,
                                    .core = Core, .tf, .cond, .drawCluster)
}

#---- getPolys is a function that makes convex polygons for clusters
#  Usually called from mapCluster, used inernally
#   .points is the SpatialPointdataFrame with cases
#   .cluster is the column with the cluster identification as an integer
#   .dis is the distance used for DBSCAN in units of .points
# Returns polygons that are convex hulls of each cluster as SpatialPolygonDataframe with same CRS as .points
getPolys <- function(.points, .cluster, .dis) {
    .cluster <- enquo(.cluster)
    stopifnot(class(.points) == "SpatialPointsDataFrame")
    #print("in getPolys, sorting on cluster_id")

    cluster_id <- sort(unique(.points@data[,quo_name(.cluster)],na.rm=T))  # unique cluster integers
    #print("sorted")
    cluster_id <- cluster_id[2:(length(cluster_id))]  # assumes first cluster id is non-clustered cases
    cluster_cnt <- length(cluster_id)
    if(cluster_cnt == 0)
      return(NULL)

    # make convex hulls for each cluster, called polys
    #print("Making convex hulls")
    polys <- vector("list", cluster_cnt)
    points_ft <- spTransform(.points, CRSobj = CRS("+init=epsg:2276"))

    for (i in 1:cluster_cnt){
        # get the records in the cluster
        ids <- points_ft@data[,quo_name(.cluster)] == cluster_id[i]
        geo <- points_ft[ids,]
        geo <- (rgeos::gBuffer(spgeom = rgeos::gConvexHull(geo), width = .dis))
        geo <-fortify(spTransform(geo, CRSobj = proj4string(.points)))
        geo$Cluster <- cluster_id[i]
        geo$group <- i
        polys[[i]] <- geo
     }
    polys <- do.call(rbind, polys) %>%
       mutate(group = factor(group))
     rm(geo, i, points_ft,cluster_cnt,cluster_id)
    return(polys)
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
#'   is not specified, then the object with have a CRS of EPSG:4326, the same as
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


## @seealso \code{\link[base]{agrep}} and @seealso \code\link{[utils]{adist}}.
#' Correct the spelling of Tarrant county cities
#'
#' Checks and corrects mis-spelling of Tarrant County cities using exact and
#' fuzzy matching. Within the function, the case of the city is standardized to
#' title case. Text after a comma is removed (e.g., ', Tx'). Common
#' abbreviations are expanded (e.g. Ft -> Fort, N. -> North). The fuzzy matching
#' is accomplished using the [stringdist::amatch] function with all cities found
#' in [city.names], and [other.city.names]. 
#' 
#' @param .city a character vector of city names
#' @param .small is the number (and smaller) of characters for small cities
#'   (defaults to 5).
#' @param .thresholds a numeric vector of size two for thresholds to use for
#'   the distance for small and large city names. Default is c(0.18, 0.20).
#' @param .method is the string dissimilarity measure to use. The default is
#'   "jw".
#' @param .addtional_cities can be used to correct other city names.
#'
#' @details The function's use of [stringdist::amatch] results in a vector of best
#'   matches or NA for those matches above the threshold. The Jaro-Winkler
#'   distance method is used by default. Other available methods are found at
#'   [stringdist::stringdist-metrics]. 
#'   Two different distance thresholds can be specified one for small city names
#'   (e.g., 5 or fewer characters) and another for larger city names (e.g. more
#'   than 5 characters). Generally a dist of 0.18 is good for names with 5 or
#'   fewer characters and 0.20 for names with more than 5 characters. 
#'   
#' @return A character vector with corrected names spelling in title case
#' 
#' @export
correct_city <- function(.city, .small = 5, .thresholds = c(0.18, 0.2), .method = "jw", .addtional_cities = NULL) {
  
  if(!is.null(.addtional_cities)){
     .addtional_cities <- stringr::str_to_title(.addtional_cities)
  }
  
  city_proc <- .city %>%
    # standardize the city names to title case
    stringr::str_to_title() %>%
    # remove trailing comma and anything after that, e.g. ", Tx"
    stringr::str_extract( "^[^,]*") %>%
    gsub("Other ?-?", "", .) %>% 
    stringr::str_trim(side = "both")
  
  # Expand or replace common abbreviations, use title case
  acryn <- c("^No?\\.? "         = "North ",
             "^Ft\\.? "          = "Fort ",
             "^Forth"            = "Fort ",
             "^Ftw"              = "Fort Worth",
             "^Fw$"              = "Fort Worth",
             "^Arl$"             = "Arlington",
             "^N.?r.?h$"         = "North Richland Hills",
             "^North ?Richland"  = "North Richland Hills",
             "Richland H\\.?i?$" = "Richland Hills",
             " Grdns?$"          = " Gardens",
             " Hls$"             = " Hills",
             " Vlg$"             = " Village",
             "^Alito$"           = "Aledo",
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
  small_ndx <- which(x = (nchar(city_unmatched) <= .small))
  compare_city <- all_cities[nchar(all_cities) <= .small]
  matched_ndx <- stringdist::amatch(x = city_unmatched[small_ndx], 
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
  
  
  # --- Using word distance (adist) to match city names
  # Many city names are correct and will not need to go through subsequent spelling
  # checks.  So use a subset of the city_proc for further cleaning. ndx are the
  # indices of  city names that do not match and must be processed further. Finally
  # ndx is used to save the processed city names to the city_proc vector that is
  # returned from the function.
#   ndx <- which(!city_proc %in% all_cities & !is.na(city_proc)  & (nchar(city_proc) > 5))
#   city_proc_sub <- city_proc[ndx]
# 
#   # use word distances to to determine the best match for cities.
#   # score the distances between city names to obtain a matrix
#   distances <- adist(x = city_proc_sub, y = all_cities)
# 
#   # local function to find the minimum score equal to or less than .threshold for a
#   # vector of numbers.  If it is above .threshold, then returns NA which signals no
#   # match found.
#   setNum <- function(num_vec){
#     nm <- min(num_vec)
#     nm <- ifelse(nm <= .threshold, nm, NA)
#   }
# 
#   # get the minimum score for each row of the matrix
#   mini <- apply(distances, 1, setNum)
# 
#   # local function to return the fuzzy matched city or the original city for those
#   # above the threshold.  When two cities match, the first one is used
#   return_city <- function(n){
#     if(is.na(mini[n]))
#       #ret <- city_proc_sub[n]
#       ret <- city_proc_sub[n]
#     else {
#       ret <- city.names[which(distances[n,] == mini[n])]
#       ret <- ifelse(length(ret)>1,ret[1], ret)
#     }
#   }
# 
#   # iterate through all the result scores in mini and call return_city
#   city_proc_sub <- purrr::map_chr(seq_along(mini), return_city)
# 
#   #--- Use agrep pattern matching to get a matched city,
#   for(i in seq_along(city.names)){
#     city_proc_sub[agrep(pattern = paste0("^",city.names[i]),city_proc_sub, fixed = F)] <- city.names[i]
#   }
#   # save the newly matched cities
#   city_proc[ndx] <- city_proc_sub
# 
#   return(city_proc)
# }
# 
