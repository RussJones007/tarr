# ------------------------------------------------------------------------------------------------------------------->
# Script: clustered_class.r
# Description:
#    Defines the clustered class used whn scanning sf point objects for clustering.
#    This script was created from functions moved from the GeoFunction.r file. 
# 
# ------------------------------------------------------------------------------------------------------------------->
# Author: Russ Jones
# Created:  April 17, 2026
# ------------------------------------------------------------------------------------------------------------------->

#' Construct a clustered object
#'
#' @description
#' Internal low-level constructor for objects of class `"clustered"`. A
#' `clustered` object is an `sf` point object with clustering results stored in
#' required columns and attributes. This constructor performs only minimal
#' checks; use `validate_clustered()` to enforce the full class contract.
#'
#' @param x An `sf` object with `POINT` geometry.
#' @param scan_object The clustering result returned by the selected
#'   `dbscan`-family algorithm.
#' @param eps_unit A character scalar giving the linear unit used for distance
#'   calculations (for example, `"feet"` or `"meter"`).
#' @param ... Additional named attributes to attach to `x`.
#'
#' @return An object of class `c("clustered", class(x))`.
#'
#' @keywords internal
new_clustered <- function(x, scan_object, eps_unit, ...) {
  checkmate::assert_class(x, "sf", .var.name = "x")
  
  attr(x, "scan_object") <- scan_object
  attr(x, "eps_unit") <- eps_unit
  
  extra_attrs <- list(...)
  if (length(extra_attrs)) {
    for (nm in names(extra_attrs)) {
      attr(x, nm) <- extra_attrs[[nm]]
    }
  }
  
  class(x) <- c("clustered", class(x))
  x
}


#' Validate a clustered object
#'
#' @description
#' Internal validator for objects of class `"clustered"`. A valid `clustered`
#' object must be an `sf` object with `POINT` geometry, contain the required
#' clustering columns, and carry the attributes added by `scan_cluster()`.
#'
#' @param x An object to validate.
#'
#' @return `x`, invisibly, if validation succeeds; otherwise an error is thrown.
#'
#' @keywords internal
validate_clustered <- function(x) {
  checkmate::assert_class(x, "clustered", .var.name = "x")
  checkmate::assert_class(x, "sf", .var.name = "x")
  
  checkmate::assert(
    checkmate::check_true(
      all(sf::st_is(sf::st_geometry(x), "POINT"))
    ),
    .var.name = "x",
    .fmt = "All geometries in a 'clustered' object must be of type 'POINT'."
  )
  
  required_cols <- c("cluster")
  missing_cols <- setdiff(required_cols, names(x))
  if (length(missing_cols)) {
    rlang::abort(
      paste0(
        "A 'clustered' object is missing required column(s): ",
        paste(missing_cols, collapse = ", "),
        "."
      )
    )
  }
  
  s_obj <- scan_object(x)
  if (is.null(s_obj)) {
    rlang::abort("A 'clustered' object must have a non-NULL 'scan_object' attribute.")
  }
  
  eps_unit <- scan_unit(x)
  #eps_unit <- attr(x, "eps_unit", exact = TRUE)
  # if (is.null(eps_unit) || length(eps_unit) != 1 || is.na(eps_unit)) {
  #   rlang::abort(
  #     "A 'clustered' object must have a non-missing scalar 'eps_unit' attribute."
  #   )
  # }
  
  if (!is.numeric(x$cluster) && !is.integer(x$cluster) && !is.factor(x$cluster)) {
    rlang::abort("The 'cluster' column in a 'clustered' object must be numeric, integer, or factor.")
  }
  
  # if (!is.character(x$point_type) && !is.factor(x$point_type)) {
  #   rlang::abort("The 'point_type' column in a 'clustered' object must be character or factor.")
  # }
  # 
  invisible(x)
}


#' Reconstruct a clustered object after manipulation
#'
#' @description
#' Internal helper to restore the `clustered` class and key attributes
#' after operations that may drop them (for example, dplyr verbs).
#'
#' @param x A modified object derived from a `clustered` object.
#' @param template A valid `clustered` object to take class and attributes from.
#'
#' @return `x` with class and attributes restored, if still structurally valid.
#'   Otherwise, `x` is returned with the `clustered` class dropped.
#' @keywords internal
reconstruct_clustered <- function(x, template) {
  # Restore basic sf class if lost
  if (!inherits(x, "sf") && inherits(template, "sf")) {
    # If geometry column survived, let sf rebuild; otherwise just drop clustered
    if ("geometry" %in% names(x)) {
      x <- sf::st_as_sf(x)
    } else {
      return(x)  # no geometry → cannot be clustered
    }
  }
  
  # Required columns must still be present
  required_cols <- c("cluster", "point_type")
  if (!all(required_cols %in% names(x))) {
    # Not a valid clustered object anymore; drop class
    class(x) <- setdiff(class(x), "clustered")
    return(x)
  }
  
  # Restore class and key attributes
  class(x) <- class(template)
  
  for (nm in c("scan_object", "eps_unit")) {
    attr(x, nm) <- attr(template, nm, exact = TRUE)
  }
  
  # Optional: revalidate; if it fails, drop class
  out <- try(validate_clustered(x), silent = TRUE)
  if (inherits(out, "try-error")) {
    class(x) <- setdiff(class(x), "clustered")
  }
  
  x
}



## s3 method for class 'clustered_dbscan'
#' Print a clustered object
#'
#' @param x A `clustered` object like that returned by `scan_cluster()`.
#' @param ... Unused.
#'
#' @return The input object, invisibly.
#' @exportS3Method base::print clustered
print.clustered <- function(x, n = 7, ...) {
  validate_clustered(x)
  
  so <- scan_object(x)
  eps_unit <- scan_unit(x)
  n_obs <- nrow(x)
  
  crs_obj <- sf::st_crs(x)
  crs_label <- if (!is.na(crs_obj)) {
     if (!is.null(crs_obj$input)) crs_obj$input else crs_obj$wkt
   } else {
     "NA"
   }
  # 
  method_class <- class(so)[1]
   method_label <- switch(
     method_class,
     "dbscan_fast" = "DBSCAN",
     "optics"      = "OPTICS",
     "hdbscan"     = "HDBSCAN",
     toupper(method_class)
   )
   
  n_clusters <- tryCatch(dbscan::ncluster(so), error = function(e) NA_integer_)
  n_noise <- tryCatch(dbscan::nnoise(so), error = function(e) sum(x$cluster == 0, na.rm = TRUE))
  
  # cluster_tab <- sort(table(x$cluster), decreasing = FALSE)
  # cluster_tab <- cluster_tab[names(cluster_tab) != "0"]
  
  cat("\n<clustered>")
  writeLines(c(paste0(method_label, " clustering for ", nobs(so), " observations."),
             paste0("Containing ", ncluster(so), " cluster(s) and ",  nnoise(so), " noise points."),
             cat("\n"))
  )
  NextMethod(n = n)
  invisible(x)
}

#' @param object 
#' @param background for clustered objects, you can prive a maptile as background. 
#' @param ... 
#'
#' @return for clsutered object, a ggplot with the clustered points.
#' @exportS3Method ggplot2::autoplot clustered
autoplot.clustered <- function(object, background = NULL, ...){
  so  <- scan_object(object)
  cluster_vec <- so$cluster[so$cluster > 0]
  cluster_count <-  c("noise" = sum(so$cluster == 0), 
                      cluster_vec |> tabulate() |> set_names(unique(cluster_vec) |> sort())
  )
  
  capt <- clustered_credits(object)
  
  object <- object |> 
    mutate(#point_size = ifelse(cluster == 0, .72, 0.73),
      cluster = factor(cluster))
  
  # create convex hulls of the clusters
  hulls <- get_cluster_polys(points = object, cluster = cluster, ...)
  
  layers <- list()
  if( !is.null(background)) {
    layers <- c(layers, list(tidyterra::geom_spatraster_rgb(data = background)))
  }
  
  layers <- c(layers, list(
    ggplot2::geom_sf(
      mapping = aes(shape = ifelse(cluster == "0", "noise", "cluster")), 
      size = 2.5), 
    geom_sf(data = hulls[-1, ], alpha = .2)
  ))
  
  # find specific arguments for ggplot that may have been passed
  args <- list(...)
  pl <- ggplot(object, aes(fill = cluster, 
                           color = cluster)) +
    layers +
    scale_shape_manual(values = c("noise" = 0, "cluster" = 19), ) +
    scale_size_manual(values = c("noise" = .25, "cluster" = 1)) +
    ggplot2::scale_color_discrete(name = "Cluster (Count)",
                                  labels = paste0(names(cluster_count), " (n = ", cluster_count, ")" )) +
    ggplot2::scale_fill_discrete() + #values = tarr.brand::tc_palettes$discrete$primary, 
    guides(fill = "none", size = "none") +
    labs( shape = "Cluster/Noise",
          caption = capt)+
    theme(plot.caption = element_text(hjust = 0),
          plot.caption.position = "plot") +
    map_theme
  
  if("title" %in% names(args))    pl <- pl + labs(title = args[["title"]])
  if("subtitle" %in% names(args)) pl <- pl + labs(subtitle = args[["subtitle"]])
  #scale_color_manual(values = tarr.brand::tc_palettes$discrete$primary)
  return(pl)
}



#' Credits for Clustered Object
#' 
#' Returns the method and important parameters like the method, minPts and eps (epsilon)
#' as a formatted string. The plot.clustered() and autoplot.clustered() functions call this function
#' to get the caption information in the plot. 
#' @param x  is the clustered object
#'
#' @returns a character string with the information regarding the method and results
clustered_credits <- function(x){
  so <- scan_object(x)
  eps_unit <- scan_unit(x)
  cit <- citation(package = "dbscan")[[1]]$title |> 
    str_remove("\\n.*") |> 
    #str_replace("\\n", " ") |> 
    #str_remove_all("[:punct:]") |> 
    str_replace(pattern = "dbscan ", replacement = "dbscan: ")
  
  capt <- paste0("Method: ", toupper(class(so)), ", minPts=", so$minPts)
  if(any(class(so) %in% c("optics", "dbscan_fast"))) capt <- paste0(capt, ", eps=", format(so$eps, big.mark = "," ), " ",eps_unit)
  capt <- paste0(capt, ", clusters=", dbscan::ncluster(so), "\n", cit)
  
  return(capt[max(length(capt))])  # return the last entry
}

# plot method for creating map with clusters shown
#' Plot Clustered Object
#' 
#' Creates a plot of a 'clustered' object as created by [scan_cluster()]. The engine argument determines how the map
#' is drawn as either an interactive tmap object or base plot object. 
#' This function differs in the typical plot functions as it can create and return a tmap. Other base R plotting
#' functions like points() are not used after calling this function.  However, you can customize the map further using 
#' tmap functions. By default the plot will be interactive using the Stadia service and the Alidade Smooth tile set.
#' **FUTURE IMPLEMENTAITON** selecting base map capability.
#' 
#' @param x a clustered_dbscan object.
#' @param main is the title as used in the regular plot function.
#' @param alpha is the transparency used for the cluster polygons.  The default is 0.4
#' @param background if a SpatRaster like from the maptile package then used as the background, otherwise a 
#' character vector of map tile services
#'   recognized by [tmap::tmap_providers()].  If the vector is named then that name will be shown in the layer widget.
#' @param engine a character string that is either "tmap" ot "base"   
#' @param map_credit is the credit for the background tile service 
#' @param ... additional arguments that such as column names in x to use for the point label, or that may be passed to
#'   other functions., For example ratio = between 0 to 1 changes the concavity or convex of the the cluster polygons.
#'
#' @return a tmap object.
#' @examples
#'  border <- load_tarrant_spatial("tarrant_border")    
#'  foo <- synthetic_outbreak[synthetic_outbreak$condition == "fooflu",] |> 
#'      sf::st_as_sf(coords = c("lon", "lat")) 
#'      
#'  foo_hdbscan <- scan_cluster(points = foo, minPts = 7, method = "HDBSCAN")    
#'  
#'  # interactive leaflet plot of clusters'  
#'  plot(foo_hdbscan, main = "Foo Flu Density Clusters")  
#'      
#' @exportS3Method base::plot clustered
plot.clustered <- function(x, main = NULL, 
                           alpha = 0.45, 
                           background = c("Esri.WorldStreetMap",
                                          "OpenStreetMap", 
                                          "Stadia.AlidadeSmooth",
                                          "Stadia.StamenToner",
                                          "OpenStreetMap.HOT",
                                          "USGS"), 
                           engine = c("tmap", "base"),
                           map_credit = NULL,
                           ...){
  
  # Make a title with the x variable if main is null
  if( is.null(main)) main <- paste(deparse(substitute(x)), "Clusters and Cases")
  
  # route additional user arguments to appropriate functions
  dots <- list(...)
  tm_symbols_user <- dots[names(dots) %in% names(formals(tmap::tm_symbols))]
  tm_polygon_user <- dots[names(dots) %in% names(formals(tmap::tm_polygons))]
  poly_user    <- dots[names(dots) %in% "ratio"]
  
  # Caption information with calling information
  params <- scan_object(x)
  credit <- clustered_credits(x)
  if(! is.null(map_credit)) credit <- paste(credit, "\n", map_credit)
  
  # labels for clusters, no label for noise points
  engine <- match.arg(engine)
  
  
  if(ncluster(params)){
    cluster_vec <- params$cluster
    cluster_count <-  (cluster_vec ) |> 
      tabulate() |> 
      set_names(unique(cluster_vec[cluster_vec > 0]) |> 
                  sort() )
    # labels for clusters to include counts in parenthesis
    cluster_label <- paste0(names(cluster_count), " (n = ", cluster_count, ")" )
  } else {
    cluster_label = "No Clusters"
  }
  
  
  # Simplify the cluster argument as a factor
  x <- mutate(x, cluster = factor(cluster),
              alpha = ifelse(point_type != "core", 0, 1)) |> #,
    filter(! is.na(cluster))
  
  orig_pal <- palette("Tableau 10")
  withr::defer(palette(orig_pal))
  points_pal <- palette.colors(palette = "Paired", n = ncluster(params), recycle = TRUE)
  
  if(ncluster(params)) {
    points_pal <- set_names(points_pal, seq(1, ncluster(params, 1)))
  } else {
    points_pal <- double(length = 1) |> 
      set_names("No Clusters")
    points_pal[] <- NA
  }
  
  #  points_pal <- set_names(points_pal, cluster_label)
  
  polys_pal <- points_pal  # remove color for noise points as they will not be shown as a cluster
  
  # get polygons for the clusters, remove the first polygon as it is noise points
  buf_dist <- set_units(x = 300, value = ft)
  # poly_args <- list(points = x, cluster = cluster, dist = buf_dist) |> 
  #   utils::modifyList(poly_user);
  # rlang::exec(get_cluster_polys, !!!poly_args)
  polys <- get_cluster_polys(points = x, cluster, dist = buf_dist, poly_user) 
  polys <- polys[-1, ] 
  if(nrow(polys)){
    polys <- cbind(polys, cluster_label)
    polys$cluster <- droplevels(polys$cluster)
  }
  
  # check and use background if present and a spatRaster
  t_ret <- if(! is.null(background) & inherits(background, "SpatRaster")) {
    tmap_mode("plot")
    tm_shape(background) + tm_rgb(col_alpha = 1)
    
  } else if(is.character(background)) {
    layers <- background[background  %in% tmap::tmap_providers()]
    if(length(layers) != length(background)){ 
      warning("One or more tile providers in the background argument is not recognized by TMap")
    }
    assert_that(length(layers) > 0, 
                msg = "None of the values in the background argument are recognized as valid")
    tmap_mode("view")
    tm_basemap(background)
  }
  
  # default arguments for tm_polygon
  tm_polygon_args <- list(
    fill = "cluster",
    fill.scale = tmap::tm_scale_categorical(
      values = polys_pal,
      labels = cluster_label),
    fill.legend = tm_legend(title = "Cluster (n)"),
    col  = "cluster",
    col.scale = tmap::tm_scale_categorical(values = polys_pal),
    fill_alpha = alpha,
    popup.vars = c("Cluster:" = "cluster_label"),
    fill_alpha.legend = tm_legend(show = FALSE),
    col.legend = tmap::tm_legend_hide()
  ) |> 
    utils::modifyList(tm_polygon_user)
  
  # do not draw polygons if no clusters are present
  if(ncluster(params))  {
    t_ret <- t_ret +
      tmap::tm_shape(polys, name = "Cluster", is.main = FALSE) +
      do.call(tm_polygons, args = tm_polygon_args)
  }
  
  #default arguments for tm_symbols
  tm_symbols_args <- list(
    fill             = "cluster",
    fill.scale       = tm_scale(points_pal),
    fill_alpha       = alpha,
    fill.free        = FALSE,
    fill.legend      = tm_legend(show = FALSE),
    fill_alpha.scale = tm_scale(values = c(0,1)), 
    fill_alpha.legend = tm_legend(show = FALSE),
    shape            = "point_type",
    shape.scale      = tmap::tm_scale_categorical(values = c("core" = 21, "noise" = 0)),
    shape.legend     = tm_legend(title = "Point Type", show = TRUE),
    size             = "point_type",
    size.scale       = tmap::tm_scale_categorical(values = c("core" = 0.8, "border" = 0.8, "noise" = 0.65)),
    size.legend      = tm_legend(show = FALSE),
    col              = "cluster",
    col.scale        = tm_scale(points_pal), 
    col.free         = FALSE,
    col.legend       = tm_legend(show = FALSE)
  ) |> 
    utils::modifyList(tm_symbols_user)
  
  
  t_ret <- t_ret +
    tm_shape(x, name = "Cases", is.main = FALSE) +
    do.call(tmap::tm_symbols, tm_symbols_args)
  
  
  t_ret <- t_ret +
    tm_borders(fill.legend = tmap::tm_legend_hide()) +
    tm_title(text = main) +
    tmap::tm_credits(text = credit, 
                     size = .8, 
                     color = "grey30",
                     position = tmap::tm_pos_out(cell.h = "center", cell.v = "bottom"))+
    tmap::tm_layout(scale = 0.80, text.fontfamily = "serif",
                    legend.frame = FALSE,
                    frame = FALSE
    )
  
  return(t_ret)
  
  # Get bounding box for the main object
  # bbox <- st_bbox(x) |> sf::st_as_sfc() |> st_buffer(dist = 15)
  # Set up plot area using the main object's extent
  # plot(x = bbox, 
  #      main = main, 
  #      border = NA)
  # title(sub = tle)
  # 
  # points(x = st_geometry(x), 
  #        col = adjustcolor(cols[params$cluster + 1], alpha.f = alpha), 
  #        bg =  adjustcolor(cols[params$cluster + 1], alpha.f = alpha),
  #        cex = ifelse(x$cluster == 0, 0.5, 1.25), 
  #        pch = ifelse(x$cluster == 0, 16, 21))
  # 
  # 
  # plot(polys[-1, ], 
  #      col = adjustcolor(cols[2:length(cols)], alpha.f = .2), 
  #      add = TRUE, border = adjustcolor(cols[2:length(cols)], alpha.f = .5))
  
}

#' @exportS3Method dplyr::select
select.clustered <- function(.data, ...){
  class_def <- class(.data)
  s_obj <- scan_object(.data)
  ret <- NextMethod()
  class(ret) <- class_def
  attr(ret, "scan_object") <- s_obj
  ret
}
  
  
#' Subset clustered objects
#'
#' @param x A `clustered` object.
#' @param i Row indices.
#' @param j Column indices.
#' @param drop Logical, ignored (always `FALSE` for sf-like objects).
#'
#' @return A `clustered` object if the required structure is preserved;
#'   otherwise an `sf` (or data.frame) with the `clustered` class dropped.
#' @export
`[.clustered` <- function(x, i, j, drop = FALSE) {
  # Perform the underlying subset (let sf/data.frame do the work)
  #nx <-  sf:::`[.sf`(x, i, j, drop = drop)
  nx <- NextMethod()
  
  # If we only subset rows (missing j), try to preserve clustered class
  if (missing(j)) {
    nx <- reconstruct_clustered(nx, template = x)
    return(nx)
  }
  
  # Column subsetting:
  # Only preserve clustered if required columns and geometry are still present
  required_cols <- c("cluster")
  has_required <- all(required_cols %in% names(nx))
  has_geom <- inherits(nx, "sf") || "geometry" %in% names(nx)
  
  if (has_required && has_geom) {
    nx <- reconstruct_clustered(nx, template = x)
  } else {
    # Drop clustered class
    class(nx) <- setdiff(class(nx), "clustered")
  }
  
  nx
}