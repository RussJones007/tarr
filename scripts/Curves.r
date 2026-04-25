# ===============================================================================================>
# Curves.r
# Defines functions to make epi curves with different time scales
#
# Main user function is epi_curve
#
# Created 4/5/19
# last modified 4/22/19
# Cleaned and revisions March 12, 2022
# Revised April 2023 to use incidence2 package
#
# R Jones
# ===============================================================================================>

# ==== incidence function for data frames ====
#' Incidence object from a data frame
#' 
#' SUPERSEDED by package incidence2.  Use the [incidence2::incidence()] function
#' Use unquoted field names for dates and groups that are part of a data frame.
#' This is a convenience function to create an incident object using the tidyverse syntax. 
#' In other words, the date and grouping arguments do not have to be referenced by df$date. 
#' 
#' @param df is the data frame with the date_fld (required) and the groups
#'   (optional) fields
#' @param date_fld is the field/column that has the actual date values.  This
#'   must contain Date or POSIXct or Date class data.
#' @param groups an optional argument naming the field/column with groups. When
#'   present incidence is calculated for each group.
#' @param ... additional arguments passed to incidence.Date
#' @return an incident object
#' @export
incidence.data.frame <- function(df, date_fld, groups, ...) {
  
  date_fld <- enquo(date_fld)
  dt <- pull(df, !!date_fld)
  msg <- base::missing(groups)
  gr = NULL
  if(!base::missing(groups)){ 
    groups <- enquo(groups)
    gr <- pull(df, !!groups)
  }
  return(incidence2::incidence(dates = dt, groups = gr, ...))
}


#' Decide on showing facets
#'
#' Internal function that returns TRUE if grps in the data frame has more than
#' one unique value.
#' @param df the data frame
#' @param grps column in df that contains grps, if anu
#' @return TRUE or FALSE
#' @keywords internal
show_facets <- function(.df = NULL, grps = NULL) {
  if(is.null(.df) || !is.data.frame(.df)){
    stop(".df must be a data frame")
  }
  ret  <-  FALSE
  #browser()
  if(!rlang::quo_is_null(grps)){
    ret <- (dplyr::pull(.data = .df, !!grps) %>% unique() %>% length() > 1)
  }
  return(ret)
}

# ==== Function epi_curve ====
#' Create epi curves with different date scales.
#'
# Quickly and easily create an epi curve (column chart) plot. Uses a data frame with a date field.
# Date intervals such as day, week, month, etc may be specified. Grouping can be done based on
# categorical columns in the data frame.
# 
# @param df is the data frame with the incidence data.
# @param dateFld is the unquoted date field in .df that contains the dates of interest
# @param groupFld is the unquoted field name in .df that contains the group(s)
# or condition(s) in the epi curve plot. NULL is the default for no groups.
# Specifying a group enables the ability to return faceted plots.  The
# .groupFld is used to title the plots by facet, or in the legend when .facet
# is FALSE (default) and more than one group is found in the field. When
# .groupFld has only one condition, that value will appear in the plot title.
# Passing NULL (the default) to .groupFld results in a general epi curve
# without the condition named in the title.
# @param interval is the interval to aggregate records for the epi curve. This
#   can be "day", "week", "epiweek" (default),"month",  "quarter, or "year.
#   Note that "week", results in ISO weeks that begin on Monday, instead of
#   Sunday. See the [incidence2::incidence()] Arguments section for more details on
#   this parameter.
# @param dateBrks is a string specifying the frequency of the dates on the
#   x-axis date values.
#   Enter any text as used by [base::seq.Date]. The default is "week".
# @param dateFormat is a character vector formatting the date on the x-axis. The NULL (default)
#   will cause the function to select a format based on the .dateBrks argument
# @param fill is the color to fill the bars (defaults to navy).  The number of colors in .fill
#   should match the number of unique values in the .groupFld parameter. If not, then the 
#   Navy is used by default. 
# @param facet if TRUE returns faceted plots when .groupFld contains more than one unique value.
#   The default is to create facets when the unique values of .groupFld is greater than 1.
# @param th is the theme to use in drawing the plot.  Defaults to ggthemes:theme_tufte().
#
# @return a ggplot with the Epi Curve represented as columns.  For data sets
#   covering more than one year,the plot has the year shown in the background
#   with the number of cases listed in parenthesis. If a grouping field is
#   passed to the function and .facet is TRUE, then the returned item will be a
#   faceted plot with each group label in the facet strip. It is possible to
#   pass a single group in which case the condtion name will be shown in the
#   chart title.  When facets are shown the condition is shown in the facet
#   label.
#
# @details The returned ggplot is a list that  has the processed data frame in the "data" field and
# consists of fields: Dates (x-axis), Group, and Cases (y-axis). 
# @seealso [seq.Date] Details for how the .dateBrk argument can be specified.
# @seealso [incidence2::incidence] Notes section how an interval may be specified.

# @export
# epi_curve <- function(data, dateFld, groupFld = NULL, interval = "epiweek",
#                       dateBrks = "week", dateFormat = NULL, palette = "navy",
#                       facet = show_facets(data, groupFld), 
#                       th = ggthemes::theme_tufte(ticks=F)){
#   
#   incd <- incidence(x = data, 
#                     date_index = dateFld, 
#                     groups = groupFld, 
#                     interval = interval)
#   if(!is.null(groupFld) & groupFld %in% names(data)){
#     n_color <- data[[groupFld]] |> unique() |> length()
#   } else {
#     n_color = 1
#   }
#   
#   ret <- plot(incd, angle = 90, palette = palette(n_color),) +
#     #scale_x_date(name = dateBrks, date_breaks = dateBrks) +
#     th
#   return(ret)
#   # test variables
#    #.df <- dis %>% filter(Condition == "Shigellosis")
#    # .df <- dis 
#    #.dateFld  <- expr(Onset)
#     #.groupFld <- expr(Condition)
#    # .groupFld <- NULL
#    # .interval = "MMWRweek"
#    # .dateBrks = "week"
#    # .dateFormat = NULL
#    # .fill = "navy"
#    # .facet = TRUE
#    # .th = ggthemes::theme_tufte(ticks=F)
#    #rm(.df, .dateFld, .groupFld, .interval, .dateBrks, .dateFormat, .fill, .facet, .th)
# 
#   # === check fields for validity and create quosures
#   stopifnot(!is.null(data))
#   stopifnot(class(data) %in% c("data.frame", "data.table", "tibble"))
#   dateFld <- rlang::enquo(dateFld)
#   if(rlang::quo_is_null(dateFld)) {
#     stop(".dateFld must be supplied")
#   }
#   groupFld <- rlang::enquo(groupFld)
#   stopifnot(lubridate::is.Date(dplyr::pull(data, !!dateFld)))
#   stopifnot(!is.null(dateBrks))
#   stopifnot(!is.null(interval))
#   
#   # === setup a separate environment and assign needed variables
#   # This allows the returned plot to be used in lists and saved to disk
#   env <- new.env(parent = globalenv())
#   env$th <- th
#   env$interval <- interval
#   env$fill <- fill
#   #env$.facet <- ifelse(is.null(.facet), show_facets(.df, .groupFld), .facet)
#   env$facet <- facet
# 
#   # grVec is the grouping variable vector expected by the the incidence function
#   grVec <- NULL
#   if(!rlang::quo_is_null(groupFld)){
#     grVec <- dplyr::pull(data, !!groupFld)
#   }
#   
#   # === mgr is the incidence object used to aggregate cases by the passed date interval
#   mgr <- incidence2::incidence(x = data, 
#                               date_index = !!dateFld,
#                               groups   = grVec,
#                               interval = interval)
#   rm(grVec)
# 
#   # === construct the title and subtitle using the mgr incidence object
#   if(!rlang::quo_is_null(groupFld)){
#     cnt <- dplyr::pull(data,!!groupFld) %>%
#       unique() %>%
#       length()
# 
#     if(cnt > 1){
#       tle <- paste("Epi Curves by", stringr::str_to_title(mgr$interval))
#     } else {
#       tle <- paste(unique(dplyr::pull(data,!!groupFld)),
#                    "Epi Curve by", 
#                    stringr::str_to_title(mgr$interval))
#     }
#   }else {
#     tle <- tle <- paste("Epi Curve by",
#                         stringr::str_to_title(mgr$interval))
#   }
#   
#   # the sub-title lists the date range from .dateFld
#   dates <- data %>% pull(!!dateFld)
#   subTle <- paste("Dates range from",format(min(dates, na.rm=TRUE),
#                   format="%B %d, %Y"), "through",
#                   format(max(dates, na.rm=TRUE),format="%B %d, %Y"))
#   
#   env$tle   <- tle
#   env$subTle <- subTle
# 
#   # === create adjust that is used to get the center of the date interval for labeling purposes
#   # adjustments <- c(day = 0, week = 3, MMWRweek = 3, month = 14, year = 182, quarter = 44)
#   # stopifnot(.interval %in% names(adjustments))
#   # adjust <- adjustments[grep(pattern = .interval,x = names(adjustments), ignore.case = T)]
# 
#   # === construct the dfmgr, the dataframe for use in the plot
#   dfmgr <- mgr %>%
#     as.data.frame()   %>%
#     dplyr::select(-matches("weeks"), -matches("isoweeks")) %>%
#     dplyr::rename_at(1, ~"Dates") %>%
#     tidyr::gather(key = Group, value = Cases, -Dates) #%>%
#     #dplyr::mutate(Label_Dates = Dates + adjust)
# 
#   env$dfmgr <- dfmgr
# 
#   # ===  construct the yr data frame used to label a year with the number of cases in parenthesis
#   #      mini is the minimum date for the year in .df and max is the last date of the year in .df
#   #      Cases is the number of cases in that date interval
#   
#   # determine the minimum and maximum for each year represented 
#   min_max <- dfmgr %>% 
#     mutate(Year = lubridate::year(Dates)) 
#     
#   years <- range(min_max$Year) %>% 
#     purrr::map_dfr(., ~ {
#       list(mini = lubridate::make_date(year = .x),
#            maxi = lubridate::make_date(year = .x, 12L, 31L))
#     })  %>% 
#     mutate(Year = range(min_max$Year)) %>% 
#     group_by(Year) %>% 
#     summarise(
#       mini = min(mini),
#       maxi = max(maxi)
#     )
#   
#   # adjust first and last dates represented
#   years$mini[1]           <- min(dates, na.rm = TRUE)
#   years$maxi[nrow(years)] <- max(dates, na.rm = TRUE)
#   
#   
#   yr <- mgr %>%
#     as.data.frame()     %>%
#     mutate(Year = lubridate::year(dates)) %>%
#     dplyr::select(-matches("weeks"),-matches("dates"), -matches("isoweeks"))     %>%
#     tidyr::gather(key = Group, counts, -Year)   %>%
#     dplyr::group_by(Group, Year) %>%
#     dplyr::summarise(Cases = sum(counts)) %>% 
#     dplyr::ungroup() %>% 
#     dplyr::left_join(years, by = "Year")
#       
#       
#    env$yr <- yr
# 
#     # === rect_colors is used to show years in different colors
#    rect_cols <- rect_color_calc(colors = c("#92EBB5", "#8878F0", "#EB6363") ,vec = yr$Year)
#    #rect_cols <- rect_color_calc(colors = c("EDEDED","#FFC0CB"),vec = yr$Year)
# 
#     # repeat rect_cols for each unique record in .groupFld
#     # if(!rlang::quo_is_null(.groupFld)){
#     #   rect_cols <- rep(rect_cols, length(unique(.df[,rlang::as_name(.groupFld)])))  # repeat for each group
#     # }
#     env$rect_cols <-  rect_cols
# 
#     # === brks are the x axis date breaks using the .dateBrks argument format for dates
#     dts <- dfmgr$Dates
#     #dts <- .df %>% pull(!!.dateFld)
#     brks <- seq(min(dts,na.rm=T),
#                 max(dts,na.rm=T),
#                 by = dateBrks) #+ adjust
# 
#     env$brks <- brks
# 
#     # === create the date format to be used for .dateBrks, the date_format_calc function
#     #     is internal and used as a helper or default formats to .dateBrk
#     date_frmt <- ifelse(is.null(dateFormat),date_format_calc(dateBrks), dateFormat)
#     env$date_frmt <- date_frmt
# 
#     rm(mgr,rect_cols)
# 
#   # === create mypl, the ggplot with all the needed information. A defined environment is used
#   #     to ensure the plot can be drawn outside of this function. mypl has the
#   #     actual incidence data in $data, where as $layers[[1]] and $layers[[2]]
#   #     contain the information for the background rectangles and text used for
#   #     labeling those rectangles, respectively.
#   mypl <- with(env, {
#     # === setup the background rects and the top of the graph labels. if
#     # .interval is year, then show the number of cases in each bar without the
#     # background year rectangles.
#     #browser()
#     if(interval != "year"){
#       if(facet){  # increase the rect_cols by number of facets
#         rect_cols <- rep(rect_cols, length(unique(pull(dfmgr,Group))))
#         setNames(rect_cols, length(unique(pull(dfmgr,Group))))
#       }
#       
#       if(facet & yr$Year %>% unique %>% length() > 1){
#         yearTexts <- geom_text(aes(x = mini + ((maxi-mini)/2),
#                                    y = -1,
#                                    label = paste0(Year," - (",Cases,")")),
#                                data  = yr,
#                                vjust = 1,
#                                hjust = .5,
#                                size  = 3.0,
#                                color = "Grey60", 
#                                inherit.aes = FALSE)
#         
#         rects    <- geom_rect(data = yr,
#                           mapping =aes(xmin=mini - 2,xmax=maxi),
#                           ymin  = -10,
#                           ymax  = 0,
#                           color = "Grey80", 
#                           fill = NA,
#                           #fill  = rect_cols,
#                           alpha = .35,
#                           inherit.aes = FALSE)
#       } else {
#         if(yr$Year %>% unique %>% length() == 1){
#           yearTexts <- NULL
#             # geom_text(aes(x = mini+((maxi-mini)/2), y = -1,), 
#             #                      label = "",
#             #                      data  = yr,
#             #                      vjust = 1,
#             #                      hjust = 0.5,
#             #                      size  = 3.5,
#             #                      color = "Grey40", 
#             #                      inherit.aes = FALSE)      
#           rects    <-  NULL
#           } else {
#         
#         yearTexts <- geom_text(aes(x = mini+((maxi-mini)/2), 
#                                    y = -1,
#                                    label   = Year),
#                                data        = yr,
#                                vjust       = 1,
#                                hjust       = 0.5,
#                                size        = 3.5,
#                                color       = "Grey40", 
#                                inherit.aes = FALSE)      }
#         
#         rects    <- geom_rect(data = yr,
#                               mapping =aes(xmin=mini -2, xmax=maxi),
#                               ymin  = -10,
#                               ymax  = 0,
#                               color = "Grey80", 
#                               fill = NA,
#                               #fill  = rect_cols,
#                               alpha = .35,
#                               inherit.aes = FALSE)
#       }
# 
#     } else {   # .interval is by year
#       rects <-  geom_rect(data = yr,mapping=aes(xmin=mini-2,xmax=maxi),ymin= 0,ymax=Inf,
#                           fill= "white",
#                           alpha = .15, inherit.aes = FALSE)
#       yearTexts <- geom_text(data = yr, aes(x=Year,y=Cases,label=Cases), position = position_stack(vjust = .5),
#                 hjust=0.5,size=3.5,color="white", inherit.aes = TRUE)
#     }
#     
#     # determine how the color fills of the bars will look.
#     if(length(unique(pull(dfmgr,Group))) <= 1){  # One or less in the group field
#       pal <- fill[1]
#     } else if(!facet) {   # ensure different fill colors are used when faceting is not used
#       if(length(fill) == length(unique(pull(dfmgr,Group)))){  #if number in .fill equals to unique Group then use it.
#         pal <- fill
#       } else {   # use the Incidence palete when .fill is not long enough
#       pal <-  incidence2::incidence_pal1(length(unique(pull(dfmgr,Group))))
#       }
#     } else {
#       pal <- rep(fill,length(unique(pull(dfmgr,Group))))
#     }
#     pal <- setNames(pal,unique(pull(dfmgr,Group)))
#     fill_scale <- scale_fill_manual(values = pal)
# 
# 
#     tmp <- ggplot(data = dfmgr, mapping = aes(x=Dates, y = Cases, fill = Group))+
#        geom_bar(stat = "identity",color = "white", size = .2,
#                 #fill = pal,
#                 alpha = .80,
#                 show.legend = (length(pal) > 1)) +
#        yearTexts+
#        fill_scale+
#        #scale_fill_manual(name = "Group", values = pal)+
#        #scale_y_continuous(expand = expansion(add= c(0,.1))) +
#        scale_x_date(expand = c(0,0),breaks = brks,date_labels = date_frmt)+
#        th +
#        theme(axis.text.x = element_text(angle = 90, vjust = 0.2),
#              panel.grid.major.x = element_blank(),
#              panel.grid.major.y = element_line(color = "Grey50",linetype = "dotted"),
#              panel.grid.minor = element_blank(),
#              strip.text.x = element_text(size = 11)
#        )+
#     labs(title = tle, subtitle = subTle, y = "Cases", x = stringr::str_to_title(interval))
# 
#     if(!is.null(rects)) {
#       tmp <- tmp +rects
#     }
#     
#     #if(length(unique(pull(dfmgr,Group)))){
#     if(facet){
#        tmp <- tmp+
#          facet_wrap(~Group)+
#          theme(legend.position = "none")
#      }
#      return(tmp)
#    })
#    
#    return(mypl)
# }

# === date_breaks_calc function
# Calculates and returns date breaks - to be implemented
# @param .dateBrks is a text argument like "week", "2 months" etc
# @param

date_breaks_calc <- function(){
  brks <- seq(min(yr$Year,na.rm=T) %>% paste0("01-01-",.) %>% mdy(),
              max(yr$Year,na.rm=T) %>% paste0("01-01-",.) %>% mdy(), by = .dateBrks)+adjust
}


#' calculate the colors for background stripes to add to plots
#' helper function for the epi_curve function
#' @param .colors is a vector of colors to be used
#' @param .vec is a vector, e.g., years, that will have colors defined,
#' it will always define colors for unique values of this argument.
#' @return a named vector of colors that that may be used in something like geom_rect
rect_color_calc <- function(colors = c(NA,"grey70"), vec){
  rectColLength <-length(unique(vec))/length(colors)
  rect_cols <- rep(colors, rectColLength)  # will always be the floor of rectColLength

  # add colors to rect_cols for partial units of clrs
  if(((rectColLength - as.integer(rectColLength)) * length(colors)) >= .99) {
    rect_cols <- c(rect_cols,colors[1:as.integer((rectColLength - as.integer(rectColLength)) * length(colors))])
  }
  names(rect_cols) <- unique(vec)
  rm(rectColLength)
  return(rect_cols)
}

# date_format_calc function
# helper function for the epi_curve function.
# determines the default format for the x-xis dates
# returns the text string that may be used scale_x_date
date_format_calc <- function(.dateBrks){
   formats <- list(day  = "%d-%b",
                   week = "%d-%b",
                   month = "%b",
                   year = "%Y",
                   quarter = "%b-%Y")
   dt   <- stringr::str_extract(string = stringr::str_to_lower(string = .dateBrks),
                                pattern = "(day|week|month|year|quarter)")
   return(formats[[dt]])
}

# ==== Function  Current_To_Previous_Compare =============
#' Compare year of interest to other years for a condition
#'
#' Uses a line list of cases (data frame) including date of the event, e.g.,
#' onset.  It compares the selected time period epi curve to a previous time
#' period. Outbreak periods are automatically identified through a simple
#' algorithm of using more than one IQR from the distribution of cases
#' and pulled out of the general comparison.  The background of the chart is the
#' mean (line) and and standard deviations (areas) of the time periods to
#' compare against. The time period of interest is shown as bars. Finally, if an
#' outbreak year is detected, it is shown as a separate line.
#'
#' @param .df is the data frame with the cases.
#' @param .dateFld is the unquoted date field in the dataframe.
#' @param .interval default to "MMWRweek"
#' @param .currPeriod is a lubridate interval of dates for the dates of
#'   interest. , defaults to the curent year to date. TO BE Implemented"
#' @param .prevPeriod is a lubridate interval of dates for the dates of
#'   interest. , defaults to the previous 5 years. TO BE Implemented"
#' @param .cond is a character string with the condition name.  This is used in
#'   the title of the plot.
#' @param .smooth is the span of of dates to smoth curves shown.
#' @param .type = "mean" or "median" that is used for the comparison years
#' @return A ggplot with the date period on the x-axis, number of cases
#'   on the y-axis, the mean median number of cases by date period and standard
#'   deviations or interquartile range.  If an outbreak period is present then
#'   it is shown as a separate line.
#' @export
Current_To_Previous_Compare <- function(.df, .dateFld, .interval = "MMWRweek",
                                        .currPeriod =
                                          lubridate::interval(lubridate::floor_date(Sys.Date(),unit = "year"),
                                                              lubridate::ceiling_date(Sys.Date(),unit = "year")),
                                        .prevPeriod = lubridate::interval(lubridate::int_start(.currPeriod)-lubridate::years(5),
                                                                          lubridate::int_start(.currPeriod)-lubridate::days(1)),
                                        .smooth = 5,
                                        .cond=NULL, .type = "Mean") {
  if(lubridate::int_overlaps(.currPeriod,.prevPeriod)){
    stop(".currPeriod and .prevPeriod cannot overlap")
  }
  # .df = dis %>% filter(Condition == "Salmonellosis")
  # .currPeriod = interestPeriod
  # .prevPeriod = comparePeriod
  # .smooth = 5
  # .cond = "test"
  # .dateFld = "Event.Date"
  # .interval = "week"
  #rm(.df, .currPeriod, .prevPeriod, .smooth, .cond, .interval, .dateFld)

  stopifnot(!is.null(.df))
  stopifnot(nrow(.df) > 0)
  .dateFld <- rlang::enquo(.dateFld)
  stopifnot(!rlang::quo_is_null(.dateFld))
  stopifnot(!is.null(.interval))
  .interval <- tolower(.interval)
  intrvl <- purrr::set_names(x = tolower(c("MMWRweek", "week", "month", "ISOweek", "EPIWeek", "year", "day")),
                             nm = c("MMWR Week", "Week", "Month", "ISO Week", "Epi Week", "Year", "Day"))

  # check that the type paramter is either median or mean
  .type <- stringr::str_to_title(.type)
  stopifnot(any(.type %in% c("Mean","Median")))

  # set up repsonse central tendency response and varaiance functions to use
  centVarList <- list("Mean"   = list("response"  = mean,
                                      "variance"    = sd),
                      "Median" = list("response"  = median,
                                      "variance"    = IQR))
  responsefuncs <- centVarList[[.type]]
  rm(centVarList)

 tmp <- filter(.df,!!.dateFld >  lubridate::mdy("01-01-2018"))
  # select the records to use from .df from the date intervals passed
  # add epoch variable classifying records as "current" or "previous"
  .df <- .df %>%
    dplyr::filter(!!.dateFld %within% .currPeriod |
           !!.dateFld %within% .prevPeriod) %>%
    dplyr::mutate(epoch = case_when(
      !!.dateFld %within% .currPeriod ~ "current",
      !!.dateFld %within% .prevPeriod ~ "previous",
      TRUE ~ "ERROR")) %>%
    select(!!.dateFld,epoch)

  # Create the list  "mgr" with two dataframes, "current" and "previous"
  # use incidence to group on epoch and sum the cases.
  # convert to a dataframe and move previous and current to key column "epoch"
  # add the unit variable used to group
  # split the dataframe between current and previous
  # remove the dates from current and previous that do not match the respective date intervals
  mgr <- incidence2::incidence(dplyr::pull(.df,!!.dateFld), interval = .interval, groups = dplyr::pull(.df,epoch)) %>%
    as.data.frame() %>%
    tidyr::gather(epoch, cases, -weeks, -dates) %>%
    mutate(unit = case_when(
      grepl("MMWRweek", .interval, ignore.case = TRUE) ~ lubridate::epiweek(dates),
      grepl("week",  .interval, ignore.case = TRUE)    ~ lubridate::isoweek(dates),
      grepl("month", .interval, ignore.case = TRUE)    ~ lubridate::month(dates),
      grepl("year",  .interval, ignore.case = TRUE)    ~ lubridate::year(dates),
      grepl("day",   .interval, ignore.case = TRUE)    ~ lubridate::wday(dates),
      TRUE ~ lubridate::epiweek(dates))) %>%
    split.data.frame(.$epoch) %>%
    purrr::map2(.y = list(.currPeriod,.prevPeriod),.f = function(.x,.y) filter(.x,dates %within% .y))

  # detect outlier dates  year
  outlier <- outlier_iqr_detect(.df = mgr$previous,
                                .dateFld = dates,
                                .caseFld = cases,
                                .method = date_span_classify("year"))
  outlier$outliers <- outlier$outliers %>%
    mutate(rcases <- zoo::rollmean(x = cases,k = .smooth,fill = c(NA,"extend",NA),align = "center"),
           cases = rcases)

  # previous is the previous records to build the mean and std deviation ribbon
  mgr$previous <- mgr$previous %>%
    dplyr::select(unit, cases) %>%
    dplyr::group_by(unit) %>%
    dplyr::summarise(Count  = sum(cases,  na.rm = T),
              Mean   = mean(cases, na.rm = T),
              StdDev = sd(cases,   na.rm = T),
              Median = median(cases, na.rm=T),
              IQR    = IQR(cases,  na.rm=T),
              central_measure   = responsefuncs$response(cases, na.rm = T),
              variance_measure = responsefuncs$variance(cases, na.rm = T)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      RCentral  = zoo::rollmean(central_measure,  k = .smooth,fill = c(NA,"extend",NA),align = "center"),
      RVariance = zoo::rollmean(variance_measure, k = .smooth,fill = c(NA,"extend",NA),align = "center"),
#      RMean   = zoo::rollmean(x = Mean,  k = .smooth,fill = c(NA,"extend",NA),align = "center"),
#      RStdDev = zoo::rollmean(x = StdDev,k = .smooth,fill = c(NA,"extend",NA),align = "center"),
#      RMedian = zoo::rollmean(x = Median,k = .smooth,fill = c(NA,"extend",NA),align = "center"),
#      RIQR    = zoo::rollmean(x = IQR,   k = .smooth,fill = c(NA,"extend",NA),align = "center")
)

  # construct the title and sub-title
  tle <- paste(ifelse(is.null(.cond),"",paste0(.cond,collapse=", ")),
               "Cases Compared from",format(int_start(.currPeriod),"%m/%d/%y"), "to",
               format(int_end(.currPeriod)-days(1),"%m/%d/%y"),
               "vs. Cases from",
               format(int_start(.prevPeriod),"%m/%d/%y"), "to", format(int_end(.prevPeriod),"%m/%d/%y"))
   subtle <- paste0("By ",names(intrvl)[match(.interval, intrvl)], " of ", quo_name(.dateFld))
                    #,", Last Case Date: ",
                    #format(max(pull(mgr$current, date), na.rm =T), "%m/%d/%y"))


  # determine best breaks from .interval.
  brks <- list("week" = 1:52,
               "year" = 1:12,
               "month" = 1:31,
               "MMWRweek" = 1:52,
               "day" = 1:365)
  # brks <- list("week" = list(x = 1:52, fmt = c(1:52)),
  #              "year" = list(x= 1:12,  fmt = month.abb[1:12]),
  #              "month" = list(x= 1:31, fmt = c(1:31)),
  #              "MMWRweek" = list(x=1:52, fmt = c(1:52),
  #              "day" = 1:365)


  pal          <- c("Mean"          = "Green",
                    "Median"        = "Green",
                    "1 Std. Dev."   = "Grey70",
                    "2 Std. Dev."   = "Grey90",
                    "1.5 IQR"       = "Grey50",
                    "Current Cases" = "Navy",
                    "Outbreak"      = "Red")

  plot_list <- list("previous"  = mgr$previous,
                    ".interval" = .interval,
                    "current"   = mgr$current,
                    "tle"       = tle,
                    "outlier"   = outlier,
                    "subtle"    = subtle,
                    "brks"      = brks,
                    "pal"       = pal,
                    ".type"     = .type)


  env <- list2env(plot_list)

  pl <- with(env,{
    tmp <- ggplot(previous, mapping = aes(x = as.integer(unit), y = RCentral, group = 1))

  if(.type == "Mean"){
    tmp <-  tmp + geom_ribbon(mapping = aes(x = unit,
                              ymin = ifelse((RCentral - 2 * RVariance) < 0, 0, (RCentral - 2 * RVariance)),
                              ymax = RCentral + 2 * RVariance,
                              fill = "2 Std. Dev."
                              ),
                #alpha = .2,
                #color = NA,
                inherit.aes = TRUE)
  }
  tmp <-  tmp + geom_ribbon(mapping = aes(x = unit,ymin = ifelse((RCentral - RVariance) < 0, 0, (RCentral - RVariance)),
                                          ymax = (RCentral + (RVariance * ifelse(.type == "Median",1.5,1))),
                                          fill = ifelse(.type=="Median","1.5 IQR", "1 Std. Dev.")),
                            alpha = .25,color = NA) +
        geom_bar(data = current,aes(x = unit,y = cases,group = 1, fill = "Current Cases"),
                 stat = "identity",alpha = 0.6, color = "white", inherit.aes = FALSE) +
        geom_line(aes(color = .type), size = 1, inherit.aes = TRUE) +
        scale_color_manual(values = pal,
                         name = "",
                         guide = guide_legend(reverse = FALSE))+
        scale_fill_manual(values = pal,
                        name = "",
                        guide = guide_legend(reverse = FALSE))+
        scale_x_continuous(expand = c(0,0.1), breaks = brks)+
        scale_y_continuous(breaks = function(x)unique(floor(pretty(seq(0, (max(x) + 1) * 1.1)))),
                           expand = c(0, 0)) +
      labs(title = tle, subtitle = subtle, x = .interval, y = "Cases")+
      ggthemes::theme_tufte()
      if(outlier$detected){
         tmp <- tmp+
           geom_line(data = outlier$outliers, aes(x = unit, y = cases, color="Outbreak"), inherit.aes = FALSE)
      }
    return(tmp)
  })
  return(pl)
}


# function date_span_classify takes a date and transforms it to another date
# based on the .span text argument.  For example passing "year" in span will
# return the method to take a date and that returns the  date of June 30th and year..
date_span_classify <- function(.span){
  .span <-  stringr::str_to_lower(.span)
  meths <- list(
    year      = function(dt) mdy(paste(6, 30, lubridate::year(dt),sep="-")),
    month    = function(dt) mdy(paste(month(dt),15, lubridate::year(dt),sep="-")),
    week     = function(dt)  week(dt) <- isoweek(dt),
    mmwrweek = function(dt)  week(dt) <- lubridate::epiweek(dt),
    day      = function(dt) dt
  )
  stopifnot(.span %in% names(meths))
  return(meths[[.span]])
}

# internal function to detect outliers using several methods.
# currently only uses those years above the inner-quartile range
# outlier identification is returned as a list
# .df is the dataframe with the dates and case count fields
# .dateFld is the unquoted field holding the dates
# .caseFld is the unqupted field name with case counts
outlier_iqr_detect <- function(.df, .dateFld, .caseFld, .method){
  stopifnot(!is.null(.df))
  .dateFld <- rlang::enquo(.dateFld)
  stopifnot(!rlang::quo_is_null(.dateFld))
  .caseFld <- rlang::enquo(.caseFld)
  stopifnot(!rlang::quo_is_null(.caseFld))

  # use the records with dates and and case count fields selected
  dflim <- .df  %>%
    select(dates = !!.dateFld,cases = !!.caseFld)

  # determine outbreak date ranges
  # goal is to remove the outbreak date ranges from previous
  # Method 1 - median and IQR
  # using previous do quartiles of case counts.  This is done by different date spans
  # days, weeks, mponths and year
  # e.g.,  aggregate by year and look at distribution.
  # This implies several years are needed to ID an outlier
  # Outlier to be indeitifed as 1.5 IQR greater than the upper quartile

  quart <- function(.method){
    unitDf <- dflim %>%
      mutate(Unit = .method(dates)) %>%
      group_by(Unit) %>%
      summarise(cases = sum(cases)) %>%
      ungroup()

    outlim <- quantile(pull(unitDf,cases),probs = seq(0,1,0.25))
    iqr <- IQR(x = pull(unitDf,cases),na.rm = TRUE)
    c_value <- outlim[3]+(iqr*1.5)
    names(c_value) <- ""
    outlier_unit <- unitDf %>%
      filter(cases > c_value)
    outlier <- .df %>%
      filter(.method(dates) %in% outlier_unit$Unit)

    rm(unitDf,iqr)
    return(list(detected = (nrow(outlier) > 0),
                quartiles = outlim,
                critical_value = c_value,
                outliers = outlier))
  }

  return(quart(.method = .method))
}


# old code
# Current_To_Previous_Compare <- function(.df, .dateFld, .interval = "weeks",
#                                         .currPeriod =
#                                           lubridate::interval(lubridate::floor_date(Sys.Date(),unit = "year"),
#                                                               lubridate::ceiling_date(Sys.Date(),unit = "year")-lubridate::days(1)),
#                                         .prevPeriod =
#                                           lubridate::interval(lubridate::floor_date(Sys.Date(),unit = "year") - lubridate::years(5),
#                                                               lubridate::ceiling_date(Sys.Date(),unit = "year")-lubridate::years(1)-1),
#
#                                         .cond=NULL) {
#
#   stopifnot(!is.null(.df))
#   env <- new.env(parent = globalenv())
#   .dateFld <- rlang::enquo(.dateFld)
#   stopifnot(!rlang::quo_is_null(.dateFld))
#
#   # Construct a vector of weeks for the years involved including missing weeks
#   # zeroes are entered for weeks and years without any cases
#   # so that the mean and std dev are  properly calculated
#   wks <- .df %>%
#     dplyr::mutate(Year = lubridate::year(!!.dateFld),
#                   Week = mmwrWeek(!!.dateFld)) %>%
#     dplyr::filter(Year >= max(Year) - 5) %>%   # use the latest five years
#     dplyr::group_by(Year, Week) %>%
#     dplyr::summarise(Cases = n()) %>%
#     tidyr::spread(key = Week, value = Cases, fill = 0) %>%
#     tidyr::gather(key = Week, value = Cases, -Year) %>%
#     dplyr::mutate(Week = as.numeric(Week))
#
#   # detect previous outbreak years  - very simple algorithm looking for outliers
#   # use 1 IQR above the 75th percentile
#   # the yrs varbable will hold those years that are considered outliers
#   yrs <- wks %>%
#     dplyr::filter(Year < max(wks$Year, na.rm = T)) %>%  # Remove most recent year cases
#     dplyr::group_by(Year) %>%
#     dplyr::summarise(Cases = sum(Cases))
#
#   outl <-
#     ceiling(quantile(yrs$Cases)[4] + (IQR(yrs$Cases) * 1.0)) # Above this number are outliers
#   yrs <- yrs[yrs$Cases > outl, "Year"]
#   if (nrow(yrs) > 0) {
#     # now select outbreak years out of wks
#     outb <- wks %>%
#       dplyr::ungroup() %>%
#       dplyr::filter(Year %in% max(yrs)) %>%
#       dplyr::mutate(Outbreak =
#                       zoo::rollmean(x = Cases,k = 3, align = "center",fill = NA)) %>%
#       dplyr::select(-Cases)
#
#   } else {
#     outb <- wks %>%
#       ungroup() %>%
#       mutate(Outbreak = 0,
#              Year = NA) %>%
#       dplyr::select(-Cases)
#   }
#   env$outb <- outb
#   env$yrs <- yrs
#
#   # prev shows the mean, 1st and 2nd standard deviations of previous years
#   # excluding any outbreak years and the current year
#   prev <- wks %>%
#     filter(Year < lubridate::year(Sys.Date()),  # remove current year
#            !Year %in% yrs) %>%       # remove oubreak years
#     group_by(Week) %>%
#     summarise(Mean = mean(Cases),
#               SD = sd(Cases),
#               Cases = sum(Cases)) %>%
#     ungroup() %>%
#     mutate(SD = ifelse(SD < 0, 0, SD),
#            Mean = zoo::rollmean(x = Mean,k = 5,fill = "extend",align = "center"),
#            SD = zoo::rollmean(x = SD,k = 5,fill = "extend",align = "center")) %>%
#     left_join(outb, by = "Week")
#
#   env$prev <- prev
#
#   # cur is the current year for comparison
#   cur <- wks  %>%
#     filter(Year == lubridate::year(Sys.Date())) %>%
#     group_by(Week) %>%
#     summarise(Cases = sum(Cases))
#   env$cur <- cur
#   rm(cur)
#
#   # Event date to notification date delay for last 120 days of the data file
#   # repDly <-  .df %>%
#   #   filter(!!.dateFld > (max(!!.dateFld) %m-% days(120))) %>%
#   #   mutate(delays = as.integer(Notification.Date - !!.dateFld))
#   #
#   # repDly <- quantile(repDly$delays, na.rm = T)["75%"]
#   # rptLag <-
#   #   data.frame(Begin = mmwrWeek(Sys.Date() - repDly),
#   #              End = mmwrWeek(Sys.Date()))
#   # env$rptLag <- rptLag
#   # rm(repDly)
#
#   # make the plot title
#   tle <- paste(max(wks$Year),paste(.cond, sep = "", collapse = ", "),
#                "Cases Compared to Incidence from",
#                min(wks$Year),"to",max(wks$Year) - 1)
#
#   subTle <- paste("Last reported event date:",
#                   format(max(pull(.df,!!.dateFld), na.rm =T), "%B %d, %Y"))
#
#   yrRng <- max(wks$Year) - min(wks$Year) -
#     ifelse(sum(!is.na(outb$Year)), length(unique(outb$Year)), 0)
#
#   env$yrRng <- yrRng
#   env$tle <- tle
#   env$subTle <- subTle
#   rm(yrRng, tle, subTle)
#
#   # create the plot
#   ret <-  with(env,{
#     tmp <- ggplot(prev, mapping = aes(x = Week, y = Mean, group = 1)) +
#       geom_ribbon(mapping = aes(x = Week,
#                                 ymin = ifelse((Mean - 2 * SD) < 0, 0, (Mean - 2 * SD)),
#                                 ymax = (Mean + 2 * SD),
#                                 fill = "2"),
#                   alpha = .3,color = NA) +
#       geom_ribbon(mapping = aes(x = Week,ymin = ifelse((Mean - SD) < 0, 0, (Mean - SD)),
#                                 ymax = (Mean + SD),fill = "1"),
#                   alpha = .15,color = NA) +
#       geom_line(aes(y = Outbreak, color = "Outbreak"), size = 1) +
#       geom_bar(data = cur,aes(x = Week,y = Cases,group = 1,fill = "Cases"),
#                stat = "identity",alpha = .6,color = "white") +
#       geom_line(aes(color = "Mean"), size = 1) +
#       scale_color_manual(values = c("Mean" = "Green", "Outbreak" = ifelse(nrow(yrs), "Red", NA)),
#                          name = "",labels = c(
#                            paste0("Mean of ", yrRng , "\nNon-outbreak Years"),
#                            ifelse(nrow(yrs), paste0("Outbreak Year (", unique(outb$Year), ")"), ""))) +
#       scale_fill_manual(values = c("2" = "Grey80", "1" = "Grey70", "Cases" = "Navy"),
#                         name = "",labels = c("1 Std. Dev.", "2 Std. Dev.", "Current Year Cases"),
#                         guide = guide_legend(reverse = TRUE)) +
#       scale_x_continuous(breaks = 1:52,expand = c(0, .1),
#                          labels = format(mmwrWeekEnd(1:52, rep(lubridate::year(Sys.Date()), 52)), "%b-%d")) +
#       #scale_y_continuous(expand = c(0,.1))+
#       scale_y_continuous(breaks = function(x)unique(floor(pretty(seq(0, (max(x) + 1) * 1.1)))),
#                          expand = c(0, 0)) +
#       # geom_segment(data = rptLag,aes(x = Begin,xend = End,
#       #                                y = max(cur$Cases, na.rm = T) * .75,
#       #                                yend = max(cur$Cases, na.rm = T) * .75),
#       #              size = 1.25) +
#       # annotate("text",label = "Case Report Lag",x = rptLag$Begin,
#       #          hjust = 0,y = max(cur$Cases, na.rm = T) * .77,
#       #          fontface = "italic",colour = "darkred",size = 3) +
#       labs(title = tle,y = "Cases",x = "Week of Onset",
#            subtitle = subTle) +
#       #theme_few_cor() +
#       theme(axis.text.x = element_text(angle = 90, vjust = .25))
#     return(tmp)} )
#
#   return (ret)
# }
