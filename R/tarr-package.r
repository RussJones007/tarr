#' tarr
#'
#'  Functions and data sets for use by Tarrant County Public Health epidemiolog staff. Provides functions for
#'  calculating MMWR weeks, writing to and from the clipboard, processing NEDSS and EpiTrax exported files and
#'  converting EpiTrax dataframes to NBS compatible data frames so the two may be combined. Provides data sets for all
#'  confirmed and probable diseases for Tarrant County since 2005. Provides several spatial data sets in the [geo_sets]
#'  list. GIS functions include [correct_city()], scanning for clusters [scan_cluster()], and creating a map of the
#'  clusters.  Population figures for Texas and counties are made available by the tarr.pop package that is loaded when
#'  this package is started.
#'
#' @author Russ Jones <RussJones007@gmail.com>
#' @keywords package
#' @docType package
#' @name tarr
"_PACKAGE"
## usethis namespace: start
#' @importFrom dplyr filter mutate select across any_vars arrange between bind_rows bind_cols collapse coalesce case_when 
#'     count desc distinct first left_join right_join glimpse if_all if_any if_else inner_join  intersect lag last n 
#'     order_by pick pull rename rename_with rowwise slice slice_head slice_tail summarize group_by ungroup vars 
#'     summarise summarize
#' @import lubridate
#' @import tidyr
#' @import units 
#' @import assertthat
#' @import checkmate
#' @import dbscan
#' @import ivs
#' @import rage
#' @import maptiles
#' @import vctrs
#' @import ggplot2
#' @importFrom stringi stri_detect_regex stri_match_first_regex
#' @importFrom tmap tm_borders tm_bubbles tm_basemap tm_add_legend tm_compass tm_credits tm_crs tm_facets tm_fill 
#' tm_labels tm_layout tm_legend tm_logo tm_markers tm_plot tm_polygons tm_scale tm_scalebar tm_sf tm_shape tm_style
#' tm_tiles tm_title tmap_save tmap_style tmap_mode tmap_leaflet tm_dots tm_rgb tm_scale_categorical
#' tm_symbols tmap_providers tm_shape
#' @importFrom tidyterra as_sf as_spatraster geom_spatraster geom_spatraster_contour  geom_spatraster_rgb
#' @importFrom generics as.factor as.ordered
#' @importFrom lazyeval uq uqf uqs
#' @importFrom rlang enquo new_quosure eval_tidy syms quo_is_null as_name as_label is_empty is_list !!! list2
#' @importFrom incidence2 incidence
#' @importFrom purrr map map_chr map_int map_lgl set_names map_dfr map_dbl compose partial keep discard negate walk
#'  imap  map2 iwalk imap_dbl imap_lgl attr_getter
#' @importFrom ivs iv iv_pairs is_iv iv_start iv_end iv_align iv_count_includes iv_count_between iv_groups new_iv iv_locate_overlaps
#' @importFrom sf st_as_sf st_is st_read st_geometry st_transform st_crs `st_crs<-`  st_is_longlat st_coordinates st_bbox 
#' st_set_geometry st_combine st_union st_buffer st_convex_hull st_is_empty st_concave_hull 
#' @importFrom stringr str_to_title str_extract str_to_lower str_remove str_replace str_replace_all str_remove 
#' str_remove_all str_detect str_pad str_trim str_to_upper str_sub str_extract_all str_c regex str_glue 
#' @importFrom glue glue
#' @importFrom readr read_csv spec_csv cols col_character
#' @importFrom nanoparquet read_parquet write_parquet
#' @importFrom ggthemes theme_tufte theme_few theme_base theme_map theme_fivethirtyeight theme_pander theme_stata
#' theme_calc theme_excel theme_wsj tableau_color_pal tableau_gradient_pal tableau_seq_gradient_pal scale_fill_few
#' scale_fill_economist scale_fill_pander scale_fill_few scale_fill_tableau scale_fill_gradient_tableau few_pal wsj_pal

## usethis namespace: end
NULL

