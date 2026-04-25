# =============================================================================
# year_first_saturday_def.r
# Creates the 'yearInfo' internal data frame
# that contains cached informaton for years like first MMWR week end date
# and number of weeks in the year for use by
# functions in the MMWR.r script
# called by "control_def.r" which saves the data frame in sysdata.rda
#
# Created 8/15/18
# Revised 8/21/2018
# R Jones
# =============================================================================
library(dplyr)

 # function to get date of first end date for a year
CreateMmwrYearFirstEndDate <- function(yr) {

  stopifnot(is.numeric(yr))

  #Find the weekday number for the first day of yr
  day <- as.Date(paste0(yr,"-01-01")) %>%  # 1st day of the year weekday number
    lubridate::wday(.)

  endDay <- as.Date(paste(yr,1,7-day+1,sep="-"))  # Calculate Saturday's date

  #Add 7 to endDay that are days 1 through 3
  endDay[lubridate::yday(endDay) < 4] <- endDay[lubridate::yday(endDay) < 4] + 7

  # adding a number causes the date to become numeric, convert back to date
  endDay <- as.Date(endDay,origin="1970-01-01")

  rm(day)
  return(endDay)
}

# function that gets the last weekNum for each passed year and calculates the week number
CreateMmwrYearEndWeek <- function(yr){

  stopifnot(is.numeric(yr))
  lastDay <- as.Date(paste0(yr,"-12-31"))   # last day of the year weekday number

  lastSaturday <- lastDay+(7- lubridate::wday(lastDay)) # get the next or last Saturday
  # if the calculated last Saturday is the 4th or earlier, then use the previous Saturday as the last one n the year

  changeDays <- lubridate::day(lastSaturday) >=4 & lubridate::day(lastSaturday) <=7
  lastSaturday[changeDays] <- lastSaturday[changeDays] -7
  firstSaturday <- CreateMmwrYearFirstEndDate(yr)

  return(as.numeric(lastSaturday - firstSaturday)/7+1)

  #rm(lastDay,lastSaturday, yr, firstSaturday, changeDays,yrsFirstDates)

}

# 1940 was chosen because we have case counts for pertussis beginning that year
yrs <- 1940:2060
yrsFirstDates <- CreateMmwrYearFirstEndDate(yrs)
yrsWeeks      <- CreateMmwrYearEndWeek(yrs)
yearInfo      <-  data.frame(Year= yrs, FirstDate = yrsFirstDates, numberOfWeeks = yrsWeeks)
#names(yrsFirstDates) <- yrs
rm(yrs,CreateMmwrYearFirstEndDate, CreateMmwrYearEndWeek, yrsFirstDates,yrsWeeks)

