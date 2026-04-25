# ========================================================================================================================
#  Functions to work with MMWR year and weeks
# ========================================================================================================================
# Author R Jones
# Revised 8/15/18
#
# Business rule for MMWR week
# The first day of any MMWR week is Sunday. MMWR week numbering is sequential beginning with 1 and
# increment with each week to a maximum of 52 or 53. MMWR week #1 of an MMWR year is the first week of
# the year that has at least four days in the calendar year. For example, if January 1 occurs on a Sunday, Monday,
# Tuesday or Wednesday, the calendar week that includes January 1 would be MMWR week #1. If January 1
# occurs on a Thursday, Friday, or Saturday, the calendar week that includes January 1 would be the last MMWR
# week of the previous year (#52 or #53). Because of this rule, December 29, 30, and 31 could potentially fall into
# MMWR week #1 of the following MMWR year.
# =======================================================================================================================
# Addition note: Health departments usually assign an MMWR week based on a hierarchy of dates from a
# disease report or investigation including:
#    - Date of disease onset
# 	 - Date of diagnosis
# 	 - Date of laboratory result
# 	 - Date of first report to public (community) health system
# 	 - State or MMWR report date

#  ============= mmwrWeek =======
#  Calculates the MMWR week for a given date.
#  Argument taken is a date, default is today's date
#  Returns the MMWR week number.

#  mmwrWeek
#' MMWR Week
#' 
#' Functions for retrieving an MMWR week number, or dates from an MMWR week and year. The MMWR week is based on date
#' rules from the CDC. The [MMWR Week Calculation Rules](https://ndc.services.cdc.gov/wp-content/uploads/MMWR_week_overview.pdf)
#'  lists actual rules used to calculate an MMWR week. 
#' 
#' @param date a vector of dates (defaults to today())
#' @return For each function:
#' * mmwrWeek: numeric representing the MMWR week number for each date.
#' * mmwrWeekBegin: the date of the Sunday for an MMWR week number and year.
#' * mmwrWeekEnd: the date of the Saturday for an MMWR week and year.
#' * mmwrWeekMonth: returns the month number (1-12) or label (Jan-Dec) for the given week and year.
#' * mmwrYearFirstEndDate: the date of Saturday for week 1 of a given year. This date is used in
#'        calculating the MMWR week and is provided for possible use in other contexts. 

#' @examples
#' mmwrWeek(as.Date(2018-06-25))
#'
#' frDate <- as.Date("2017-01-01")
#' toDate <- as.Date("2017-12-31")
#' 
#' # mmwrWeek() function and the lubridate package epiweek() return the same information 
#' mmwrWeek(seq(from=frDate,to=toDate,by="week"))
#' epiweek(seq(from=frDate,to=toDate,by="week"))
#' 
#' # The Sunday date of the 23rd week of 2022
#' mmwrWeekBegin(weeknum = 23, yr = 2022)
#'
#' @export
mmwrWeek <- function(date=today()) {
  # ensure date is in Date format
  date <- as.Date(date)
  return(lubridate::epiweek(date))
}


#  ======== mmwrWeekEnd =====
#  Takes a MMWR week number and year, returning the date that the week ended on (Saturday)
#  The argument lengths must match
#  Arguments:
#     weekNum is the MMWR week number of interest
#     yr is the year of the WWMR week. Defaults to today's year.
#  Returns the last date of that week
#  Last modified on 3/9/2015 to handle NAs
#  Last modified 2/15/18 to recycle the yr argument
#  Revised 7/30/2018 added to tarr package
#' @param weekNum an integer of the MMWR week of interest.
#' @param yr year of the MMWR week. Defaults to today's year.  
#' @rdname mmwrWeek
#' @export
mmwrWeekEnd <- function(weekNum, yr= lubridate::year(today())){
  if(is.null(weekNum)) return(NA)
  # special case if yr is less than weekNum, recycle yr
  if(length(yr) < length(weekNum)) {
    yr <-  rep(x = yr,length.out = length(weekNum))
  }

  # ensure both arguments are the same length
  if(length(yr) != length(weekNum)) {
     stop("weekNum and yr must be the same length")
  }

  # Ensure weeks does not exceed range, still allows error when week is 53 and should not be less than 1
  if(min(weekNum,na.rm=TRUE) < 1 | max(weekNum, na.rm = TRUE) > 53){
    stop("weekNum must be greater than 0 and less than 53")
  }

  # return NA in places where no data exists
  #dt <- numeric(length(weekNum))  # dates to return, defaults to NA for missing week numbers
  dt <-NA
  recs <- !is.na(weekNum) & !is.na(yr)  #logical to select only valid data from passed vectors

  # if either argument has an NA, it will be ignored
  weekNum2 <- as.integer(weekNum[recs]) #variable to get only weekNums with non-NAs
  yr2 <- yr[recs] # years without an NA entered
  yr2 <- as.integer(yr2) # convert yr2

  endFirstWeek <- mmwrYearFirstEndDate(yr2)
  dt[recs] <-endFirstWeek + lubridate::weeks(weekNum2-1)
  class(dt) <- "Date" # convert from numeric to Date format

  return(dt)
}

#  ========== mmwrWeekBegin =======
#' @rdname mmwrWeek
#' @export
mmwrWeekBegin <- function(weekNum,yr= lubridate::year(today())){
  return(mmwrWeekEnd(weekNum,yr)-6)
}

#  =========  mmwrWeekMonth =========
# The month from the MMWR week and year
#
#' @param label FALSE (default) returns the month number otherwise an 
#' full month name as an ordered factor.
#' @param abbr TRUE (default) if label is true then the month name is abbreviated to 3 letters when label is TRUE.
#' @rdname mmwrWeek
#' @export
mmwrWeekMonth <-function(weekNum, yr=lubridate::year(today()), label=FALSE, abbr=TRUE) {

  return(month(mmwrWeekEnd(weekNum,yr),label,abbr))
}


# =========== mmwrYearFirstEndDate ======
# Date of last day in first MMWR week
# 
# Used to get the first Saturday date of  week 1 in a given year which is useful for
# calculating week numbers after this date.
# Argument:
#   - yr is the year of interest, defaults to current year.  This function uses an internal cache of dates for the first
#  Satruday of each year from 1940 through 2060.
#' @rdname mmwrWeek
#' @export
mmwrYearFirstEndDate <- function(yr=lubridate::year(today())){

  if(!any(yr %in% yearInfo$Year)){
    stop(paste("Year(s)", yr[!yr %in% yearInfo$Year],
               "not currently supported by mmwrWeek functions"))
  }

  # function to return single end date
  ret_dt <- function(syr){
    yearInfo$FirstDate[yearInfo$Year == syr]
    #yrsFirstDates[names(yrsFirstDates) == syr]
  }

  ret <- purrr::map_dbl(yr,ret_dt) %>%
    as.Date(., origin = "1970-01-01")

  return(ret)

}



# MMWR Week Business Rules documentation --------------------------------------------------------------------------

#' @title MMWR Week Calculation Rules
#' @name mmwr_week_rules
#' @description
#'   This topic documents the rules for calculating the MMWR week as from the [CDC document "MMWR
#'   Weeks"](https://ndc.services.cdc.gov/wp-content/uploads/MMWR_week_overview.pdf)
#'   
#' ## Rules
#' 'The first day of any MMWR week is Sunday. MMWR week numbering is sequential beginning with 1 and 
#' incrementing with each week to a maximum of 52 or 53. MMWR week #1 of an MMWR year is the first week of
#' the year that has at least four days in the calendar year. For example, if January 1 occurs on a Sunday, Monday, 
#' Tuesday or Wednesday, the calendar week that includes January 1 would be MMWR week #1. If January 1 
#' occurs on a Thursday, Friday, or Saturday, the calendar week that includes January 1 would be the last MMWR 
#' week of the previous year (#52 or #53). Because of this rule, December 29, 30, and 31 could potentially fall into 
#' MMWR week #1 of the following MMWR year. '
#' 
#' @details
#' To retrieve the first end date (Saturday) of the first week in the year you can use [mmwrYearFirstEndDate()].
#'
#' @seealso [mmwrWeek()], [mmwrWeekEnd()], and [mmwrWeekBegin()] 
NULL


