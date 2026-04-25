# plots.r
# 
# Functions for plotting:
#  - epi_curve, flexible epi curve plotting based on plot.incidence2
#  - compare_previous, comparing selected time period to previous time periods

# plotting an epi curve generic
#' Epidemic curve
#' 
#' Plot a epidemic curve from an incidence2 object.
#' A simple curve would show one time variable and the count of cases or outcomes. 
#' More complex curves can be created.
#' 1.  When the counting variable has more than one unique entry, a facet  is added for each unique value.
#' 2.  A grouping variable such as sex or race can show a stacked, dodge with different colors or faceted plot.
#' 3.  When combining multiple counting variables and grouping variables, there will be facets for each count
#'     variable showing the stacked or dodged carts.   When grouping variables are faceted, then the facets 
#'     becomes the product of both unique entries in the counting and grouping variables.  
#' 
#' @param x a data frame (not implemented) or incidence object
#' @param title chart title
#' @param sub_title for chart
#' @param caption caption in the chart
#' @param color_pal palette to use for columns
#' @param angle x-axis text angle
#' @param theme a theme like theme_few()
#' @param width Width of each column, defaults to 1
#' @param facet whether to facet based on the group field.  default is FALSE.
#' @param nrow number of rows if facet is TRUE
#' @param ... additional arguments passed to ggplot
#'
#' @return a ggplot
#' @export
epi_curve <- function(x, title , sub_title, caption,
                      color_pal, angle, 
                      theme, 
                      width, 
                      facet, 
                      nrow, ...){
  UseMethod("epi_curve")
}


#' @export
epi_curve.incidence2 <- function(x, title = character(0), 
                                 sub_title= character(0),
                                 caption = character(0),
                                 color_pal = few_pal(), 
                                 angle = 0, 
                                 theme = theme_few(), 
                                 width = 0.95, 
                                 facet= FALSE, 
                                 nrow= 3, ...){

  assertthat::assert_that(!is.null(theme))
  
  # MUCH OF THE FOLLOWING COE IS FROM incidence2::plot.incidence2

  # For R CMD check
  .data <- NULL
  
    # general defaults 
  border_colour = NA
  na_color     = "grey"
  alpha         = 0.7
  
    # get relevant variables
  groups       <- get_group_names(x)  
  count_values <- get_count_value_name(x)
  dates        <- get_date_index_name(x)
  count_names <- get_count_variable(tmp) |> unique() |> length()
  
  #x[[dates]] <- as.Date(x[[dates]])
  
    # check if groups is greater than one, if so use the incidence2 plot method
  if(length(groups) > 1 | count_names > 1) {
    ret <- plot(x, 
         title = title, 
         alph = alpha, 
         angle = angle, 
         nrow = 3,
         width = width, 
         colour_palette = color_pal,
         nrow) + 
      scale_y_continuous(expand = expansion(mult = c(0,0.1)))+
      theme +    # causes loss of theme elements set in incidence2:plot
      theme(legend.position = "none",
            )
    
    if(! is_empty(sub_title)) ret <- ret + labs(subtitle = sub_title)
    
    hjust <- (if(angle != 0) 1 else 0)
    ret <- ret +
      theme(axis.text.x = element_text(angle = angle, hjust = hjust))
    
    return(ret)
  }
  
  rm(count_names)
  
  # set axis variables
  x_axis <- dates
  y_axis <- count_values
  
  # determine fill values
  n_fills     <- x[[groups]] |> unique() |> length()
  if(n_fills == 0) n_fills <- 1 
  fill_colors <- color_pal(n_fills)
  
    # set fill to column that has groups
  fill = character()
  if(!is_empty(groups)) fill <- groups
  
  # add appropriate x scale
  x_scale <- switch(class(x[[dates]]),
         grates_epiweek  = grates::scale_x_grates_epiweek,
         grates_isoweek  = grates::scale_x_grates_isoweek,
         grates_period   = grates::scale_x_grates_period,
         grates_year     = grates::scale_x_grates_year,
         grates_yearmonth   = grates::scale_x_grates_yearmonth,
         grates_yearquarter = grates::scale_x_grates_yearquarter
  )
         
  date_format <- "%m/%d/%Y"
         
  ret <- ggplot(x, aes(x = .data[[dates]], y = .data[[count_values]])) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
    x_scale(format = date_format, expand = c(0,0))+
    theme 
    
  
  if(facet) {
    ret <- ret + 
      geom_col(fill = fill_colors[1]) +
      ggplot2::facet_wrap(ggplot2::vars(!!!rlang::syms(groups)),
                          nrow = nrow) +
      ggplot2::theme(legend.position = "none")
  } else { 
    ret <- ret + 
      geom_col() +
      ggplot2::aes(fill = .data[[fill]]) +
      scale_fill_manual(values = fill_colors, na.value = na_color)
  }
  
  if(! is_empty(title)) ret <- ret + labs(title = title, subtitle = sub_title, caption = caption)
  hjust <- (if(angle != 0) 1 else 0)
  ret <- ret +
    theme(axis.text.x = element_text(angle = angle, hjust = hjust))
  ret
}  
  
  

