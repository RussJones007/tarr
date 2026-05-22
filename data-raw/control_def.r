# =============================================================================
# control_def.r
#
# Created Jan 25, 2024
# R Jones
# =============================================================================
library(magrittr)
library(tidyverse)
library(readxl)
#library(tidycensus)
library(janitor)
library(arrow)
library(ivs)
library(janitor)

rm(list = ls())

# need paths defines globally for some functions to work.
# source( "R/Paths.r")
# paths <- tarr::paths_defined()
paths <- tarr:::paths_defined()
usethis::use_data(paths, internal = FALSE, overwrite = TRUE)

load_all()

# --- Convenience function to set source and url attributes for data frames
#' Set source attributes
#'
#' Attributes like the source program, url, and date created.
#'
#' @param obj
#' @param nm
#' @param url
#'
#' @return the object with the source attribute set to include name of source,
#' url, and the date the obbject was last created/revised
set_source_url <- function(obj, nm, url) {
  attr(obj, "source") <- c(
    note = nm,
    source = url,
    updated = as.character(today())
  )
  obj
}

# check files are in location expected relative to the package home directory
fls <- c(
  "paths_def.r",
  "geo_define.r",
  "misc_def.r",
  "year_first_Saturday_def.r",
  "reportNames_def.r",
  "synthetic.r"#, 
  #"pop_def.r",
  #"census_data.r"
) |> 
  {\(fl) file.path("data-raw", fl)}()


if (all(file.exists(fls))) {
  # call each script
  purrr::walk(fls, ~ {
    source(file = .x)
    print(paste("Finished", .x))
  })
} else {
  stop(paste("one of the defintion files does not exists in the location
             expected.Current folder is", getwd()))
}

rm(fls)

# load("R/sysdata.rda")
#source("data-raw/misc_def.r")

change_note <- function(df, note) {
  source <- attr(df, "source")
  source["note"] <- note
  attr(df, "source") <- source
  df
}

rm(change_note, set_source_url)

usethis::use_data(yearInfo, disease_map, field_names,city_names_spell, reportNames,
                  internal = TRUE, overwrite = TRUE)
usethis::use_data(age.groups, city.names, city.names.outside, base_maps,
                  simple_conditions, encounter_descriptions,
                  geo_sets, paths, synthetic_outbreak, tarr.filters, 
                  internal = FALSE, overwrite = TRUE)

rm(list = ls())


