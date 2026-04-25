# Age_calculation.r -------------------------------------------------------
#
# Functions for calculating and categorizing  ages.

# ==== Function age.calc ======
# Calculate age
# 
# See [age_calc()] for the updated function information. Calculates the age between start and end. If the later date is
# entered as .start, and the earlier date is entered as end, the result will be negative. This function is an alias for
# [age_calc()] which should be used instead.
#
# @param .start a Date or POSIXt object e.g., date of birth
# @param .end   The end date to calculate age.  Default is today
# @param .unit  A character string showing the age unit to use like: years (default)
# @details Units that may be used are: years, months, days, hours, minutes, or seconds. The concept of age is the
#   "floor" in the desired time unit. For example a child who is 21 months old would be 1 year old, not 1.75 years old.
#
# @return A difftime as an integer for the number of units in age
# @examples
#  age <- age.calc(as.Date("1959-07-04")) # years as of today
#' @keywords internal
#' @export
age.calc <- function(.start, 
                     .end = today(), 
                     .unit = c("year", "day", "hour", "min", "month", "second", "week")) {
    .unit <- match.arg(.unit)
    age_calc(.start, .end, .unit)
}

#' Calculate age
#'
#' @description Functions to calculate age and age groups. The age_calc() function calculates the age from start to end
#'  dates. Typically the age is calculated in years though other time units can be specified. If the end date is
#'  chronologically before the start date, the returned age is negative. The start and end date vector should be the
#'  same length or one (recycled to greater size). The [units::units] package is used to set the unit of the calculated
#'  ages. This enables easy conversion to different time units using the [units::set_units()] or the
#'  [units::`units<-`()] functions.
#'   
#'  The previous 'age.calc()' function is synonymous to age_calc() and is available for older code compatibility.
#'   
#'  The age_cat() function categorizes ages into groups that are an ordered and labeled factor. The function is
#'  specifically tailored for ages expressed in years, but can be used with other units of age. If the ages passed to
#'  the function are resulted from age_calc() the time unit for age categories will be set to that unit. Regular numeric
#'  vectors are assumed to be in a unit of 'year.'
#'
#' @param start A Date or POSIXct object e.g., date of birth
#' @param end   A Date or POSIXct of the the end date to calculate age.  The default is [today()].
#' @param unit  A character string of the age unit to use like: year (default), month, week, day, hour, min, or
#'   second. 
#'   
#' @details  The concept of age is the "floor" in the desired time unit. For example a child who is 21 months old would
#'   be 1 year old, not 1.75 years old.
#'
#' @return for age_calc(), the age as an "units" vector matching the unit parameter (e.g."year")
#' @seealso [age.groups] for a pre-defined list of age groups that are often used.
#' @export
#' @examples
#'  # create 10 random birth dates
#'  begin <- as.Date("1990-01-01")
#'  last  <- as.Date("1998-12-31")
#'  
#'  set.seed(123)
#'  birth_days <- seq(begin, last, by = "week") |>
#'       sample(size = 10, replace = FALSE)
#'  birth_days
#'  
#'  # Age in years as of Christmas 2025
#'  age_vec  <- age_calc(start = birth_days, end = as.Date("2025-12-25"))
#'  age_vec
#'  
#'  # Weeks between July 4th 1959 and 1969
#'  age_2 <- age_calc(start = as.Date("1959-07-04"), end = as.Date("1969-07-04"), unit = "week")
#'  age_2
#'  
#'  
#'  units(age_2) <- "year"
#'  age_2
#'  
age_calc <- function(start, 
                     end = today(), 
                     unit = c("year", "day", "hour", "minute", "month", "second", "week")) {
  
  assertthat::assert_that(assertthat::is.date(start) & assertthat::is.date(end))
  sized_vctrs <- vctrs::vec_recycle_common(start = start, end = end)
  if( is_empty(start) | is_empty(end)){
    warning("start or end is empty when calculating age. Vector of NA returned")
    return(rep(x = NA_integer_, times = length(sized_vctrs[["start"]])))
  }
  assertthat::not_empty(start)
  assertthat::not_empty(end)

  unit   <- match.arg(unit)
  assertthat::assert_that(assertthat::is.scalar(unit) & assertthat::is.string(unit))
  res   <- switch(unit,
                  "year"   = calc_year(sized_vctrs[["start"]], sized_vctrs[["end"]]),
                  "month"  = (as.period(interval(sized_vctrs[["start"]], sized_vctrs[["end"]]), units = "month") / months(1)) |> 
                    floor(),
                    # default
                  difftime(sized_vctrs[["end"]], sized_vctrs[["start"]], unit = unit) |> floor()) |>
    as_units(unit)
    #`attributes<-`(x = _, list(units = unit))
  return(res)
}



# internal function that calculates years between two dates, Use in age_calc
# Revised and based on code by Roman Shevtsiv at
# https://stackoverflow.com/questions/15569333/get-date-difference-in-years-floating-point
# updated that code to remove the error with vector use in an if function
# Ignores NA in either start or end
# Assumption is that start and end are the same length and is enforced by the calling function
#' @param start, end 
#' @noRd
calc_year <- function(start, end){
  #  return vector set to NA the same length as start and end
  ret <- integer(length(start))
  ret[] <- NA_real_
  
  # remove NA from being calculated
  na_lgl <- (is.na(start) | is.na(end))
  start  <- start[!na_lgl]
  end    <- end[!na_lgl]
  
  year_delta    <- lubridate::year(end)  - lubridate::year(start)
  months_delta  <- lubridate::month(end) - lubridate::month(start)
  days_delta    <- lubridate::day(end)   - lubridate::day(start)
  
  # take away one year based months and days have not occurred yet
  one_lgl       <- ((months_delta < 0) | (months_delta == 0 & days_delta < 0))
  year_delta[one_lgl]  <- year_delta[one_lgl] - 1
  
  # assign non-NA values and return
  ret[!na_lgl] <- year_delta
  return(ret)
}


# ==== Function age.cat =======
#' @keywords internal
#' @export
age.cat <- function(.ages, .lower = 0, .upper=70, .by = 10,
                    .sep = "-", .above.char = " +") {

  # check argument types
  num_args_err <- list(.ages, .lower, .upper, .by) %>% 
    rlang::set_names(names(as.list(args(age.cat))[1:4])) %>%
    purrr::map_lgl(is.numeric)
  
  if(!all(num_args_err)){
    arg_err <- names(num_args_err)[!num_args_err]
    stop('Argument: "', arg_err, '" must be numeric')
  }
  rm(num_args_err)

  if(length(.by) ==1){  # uniform span in age groups
    labs <- c(paste(seq(.lower, .upper - .by, by = .by),
                    seq(.lower + .by - 1, .upper - 1, by = .by),
                    sep = .sep),
              paste0(.upper, .above.char))
    brks <- c(seq(.lower, .upper, by = .by), Inf)
    labs <- brks |> as.age_group(sep = .sep, below = .lower, above = .above.char) |> as.character()
    ret  <-  cut(.ages, breaks = brks,
              right = FALSE, labels = labs, ordered_result = T)
      } else {  # custom age cuts being used
    # consruct label limits
    lowers <- .by[1:(length(.by) - 1)]
    uppers <- .by[2:length(.by)] - 1
    labs   <- .by |>  as.age_group(sep = .sep, below = .lower, above = .above.char) |> as.character()
    ret    <- cut(.ages, breaks = .by, right = FALSE,labels = labs, ordered_result = T)
      }
  attr(ret, "units") <- units(.ages)$numerator
  return(ret)
}

#' @param ages is a vector of integers or units.  If thisis a units vector, the return will be in that unit of time.
#' @param lower is the lowest age (default is 0) to include
#' @param upper is the max age (default is 70), above which the ages are aggregated
#' @param by may be a single number for the span of ages in each age group (e.g. '5' creates five year age groups) or a
#'   vector of cut points that defines each age group. When a single number is given, the upper age group will always 
#'   be inclusive of all ages above.  When giving a vector or cut points like 'c(5, 15, 35, 65)' the upper age group will 
#'   not be inclusive of ages above unless 'Inf' is added e.g., 'c(5, 15, 35, 65, Inf)'
#' @param sep is the separation character used in the age group label (default is "-") but may be 'through', 'thru', or
#' 'to'. 
#' @param above.char is the character used for the upper summary group.  It can be '>', '+', 'plus' and 'above'
#' @rdname age_calc
#' @return for age_cat(), a vector of age groups as an ordered factor.  If the ages argument had a units attribute, the 
#' returned factor will have the same value. 
#' @examples
#' 
#' #age_vec <- c(25, 23, 20, 1, 0, 7, 10, 50, 50, 52, 54, 56,34)
#' 
#' #default 10 year age groups
#' grp10 <- age_cat(ages = age_vec)
#' grp10 
#' data.frame(age = age_vec, grp10)
#' 
#' # special age groups with a different age separator
#' age_cat(ages = age_vec,  sep = " to ", by = age.groups$Spec)
#' 
#' @export
age_cat <- function(ages, lower = 0, upper=70, by = 10,
                    sep = "-", above.char = " +") {
 age.cat(.ages = ages, .lower = lower, .upper = upper, .by = by,
          .sep = sep, .above.char = above.char)
  }


#' Calculate the age as minimal days, months or years
#'
#' Using a start time, like a birth date and an event date the function calculates the age based on the minimal unit;
#' days, months, or years.  For example if the number of days is 31 or more but fewer than 365, the number of months are
#' calculated. Likewise if the number days is less than 31, the number of days are returned
#' @param birth
#' @param event
#' @return for calc_age_year_month_day(), a named vector with ages.  The names denote the unit; y - years, m - months,
#'   and d - days.
#' @keywords internal
calc_age_year_month_day <- function(birth, event){
  
  if(any(is.POSIXct(birth), is.POSIXct(event))) {
    birth <- as.Date(birth)
    event <- as.Date(event)
  }
  
  # get the number of days between the dates  
  days <- difftime(event, birth, units = "day")
  
  # age units logical used and the number of days for each, the names match those units passed to age_calc
  spans <- list(
    year   = days >= 365,  # years
    month  = (days >= 30 & days < 365),  # months
    day    = days < 31  # days
  ) |> 
    keep(.p = \(x) sum(x, na.rm = TRUE) > 0)
  
  # vector to translate from years, months, days to y, m, and which matches NEDSS unit coding
  # translate <- c(years = "y", months = "m", days = "d")
  
  ages <- numeric(length(birth))   # the vector to return, set to 0
  nms  <- character(length(birth)) # character vector to use for naming ages
  ages[is.na(spans$y)] <- NA_real_ 
  nms[is.na(spans$y)]  <- ""       # set the name to blank for those entries that are NA
  
  # now change all NAs in the logical span vectors to FALSE
  spans <- map(spans, function(x) replace(x, is.na(x), FALSE))
  
  # internal function to calculate the age based on the unit
  age_unit_calc <- function(s, nm){
    ages[s] <<- age_calc(birth[s], event[s], nm) 
    nms[s]  <<- nm
  }
  
  iwalk(spans, age_unit_calc)
  names(ages) <- stringr::str_to_title(nms)
  return(ages)
}
