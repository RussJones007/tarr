# ====================================================================================================
# Misc.r
# Miscellaneous variables documentation
# and functions.

# Created 8/7/18
# R Jones
# ====================================================================================================

#' File filters for the tarr package 
#'
#' A matrix that may be used when interactively choosing or list files.
#' @format matrix with entries:
#'    * NBS: NBS_*.csv
#'    * TriSano: TriSano_*.csv
#'    * EpiTrax: EpiTrax_*.csv
#'    * csv: *.csv
#'    * txt: *.txt
#'    * Excel: *.xls*
"tarr.filters"

#' City names in Tarrant County
#'
#' The city.names variable is a character vector in title case of those cities that have any area in Tarrant County.
#' @rdname correct_city
#' @seealso [correct_city()]
"city.names"

#' Some city names outside of Tarrant county
#' 
#' The city.names.outside character vector in title case for for some cities completely outside of Tarrant county, but
#' in Texas. These names are used mostly to enable fuzzy matching differentiation between the cit names inside Tarrant
#' County within the correct_city() function. This vector may be expanded in the future to include most cities in Texas.
#' @rdname correct_city
"city.names.outside"

#' Artificial disease data set
#' 
#' Simulated disease occurrence of fake diseases lorebiasis and fooflu in Tarrant county
#' during 2020. May be used to explore epi curves, geographic occurrence, etc.
#' 
#' @format fields include
#' * condition a character vector of the diseases
#' * sex female, male, unknown, or NA.
#' * outcome Recover, Death, or NA.
#' * race Asian, Black, etc.
#' * onset date of symptom onset.
#' * dob is the date of birth.
#' * lat is the latitude (y) of the case.
#' * lon is the longitude (x) of the case.
"synthetic_outbreak"


#' Simple Disease Condition Names
#' 
#' A named character vector of NBS conditions with a simplified name.  The character vector value is the original name
#' found in NBS and the 'named' part is the simplified component.  For example "Salmonellosis" =  "Salmonella,
#' non-Paratyphi/non-Typhi". This named vector used is by default in the [simplify_term()] function when renaming
#' NBS conditions.  The vector can also be used to collapse several conditions into one.  For example older West
#' Nile condition names like  West Nile Fever, West Nile Virus, Non-neuroinvasive, and West Nile Virus
#' Non-neuroinvasive disease are collapsed into West Nile non-neuroinvasive.  This enables older data in NBS with
#' previous condition names to use a common name plus shortens the name for use in report tables.
#' @format of the vector is
#' "Desired Name"  = "NBS Name"
"simple_conditions"


#' Map Tiles For Static Maps
#' 
#' A named list of map tiles previously retrieved from Google , and StadiaMaps.The Google tiles are
#' availabe in both color and black-and-white.   These map tiles are raster format that may be used in the background
#' of plot or ggplot.  All of these tiles have a crs = epsg:3857 as that is what  the map servers return.  Therefore
#' points or polygons to place on the map should be be first transformed to the same crs. 
#' Each base map can be displayed using the plot() or ggmap::ggmap() functions.
#' @examples
#' Access the color roadmap tile
#' `base_maps$google$color$roadmap`
"base_maps"

#' Encounters vector data frame
#' 
#' A data frame of case encounter descriptions from EpiTrax  Contains over 450 "encounters" aggregated from notes of
#' several epidemiology investigators.  The data frame may be used in testing text classifiers, for example the
#' [case_interview_classifier()] function that is meant to find interviews done by looking at the encounter description 
#' field. The attempt_class field is a factor of attempts and interview by note as classified by manual review.  In
#' addition to testing regular expressions to classify the encounter notes, supervised machine models can be trained and
#' then compared to the test data set by using this data frame.
#' @format
#' * Description holds the text entered into the encounter description field
#' * attempt_class is a factor of interview status - Attempted (all attempts to interview the case), Interview 
#' (actually interviewed) , Not an attempt (e.g. record review, calling IP or docotro, ROI, etc)
"encounter_descriptions"