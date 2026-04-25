# agegroup.r
# 
#
# Implementation of a S3 class age_group based on the ivs class.  Wha is meant by ag_group is an interval from a 
# young age to an older age.  Single ages can be represented as an age_group.
#
# - Added functions as.age_group and age_group_to_char that convert age groups or ivs intervals to each other. 

#' Create an interval age_group 
#' 
#' Creates an vector of age_group with minimal checking.  Other functions call this constructor where proper validation
#' occurs. All ages can be represented as "group" of ages.  For example 4 years old is easily represented as '[4,5)'.
#' 
#' @details
#' Double vectors are used.  Integers were originally used as thy are smaller and the idea that the ends and starts
#' would always be integer types.  However integer is not consistent with the concept of an interval which encompasses
#' all real numbers beginning with 'start' up to but not including 'end'.  Another very important reason to use use
#' double instead of integer is the 'Inf' is supported in doubles but not integers.  Age groups that include an age and
#' above are encoded as a right open interval using   'Inf'. For  example '85 +' is encoded as an interval like so '[85,
#' Inf)'
#' 
#' @param start,end are double vectors of the same length.  
#' @param symbols is a list of three with symbols for: separator, below, and above
#' 
#' @return an age_group object represented as intervals. The attribute "symbols" is set and is used to format the
#' age_group when printed.
#' @keywords internal
new_age_group <- function(start = NULL, 
                          end = NULL, 
                          symbols = list(below = "0-", sep = "-", above = "+")){
  if(is.null(start) & is.null(end)) return(ivs::new_iv(start, end, class = "age_group", symbols = symbols) )
  assert_that(is.double(start))
  assert_that(is.double(end))
  assert_that(rlang::is_list(symbols) & (length(symbols) == 3))
  
  ret <- ivs::new_iv(start, end, class = "age_group", symbols = symbols) 
  
  return(ret)
}

# internal convenience function to get the attribute 'symbols' list from an age_group object
get_symbols <- purrr::attr_getter("symbols")

# Changes the interval to print out as typical age groups
#' @exportS3Method
format.age_group <- function(x, ...){
  ch <- as.character.age_group(x)
  ch
}

#' @keywords internal
#' @export
vec_ptype_abbr.age_group <- function(x, ...){"aGrp"}

#' @keywords internal
#' @export
vec_ptype_full.age_group <- function(x, ...) "age_group"


# functions for restoring attributes -------------------------------------------------------------------------------

#' @keywords internal
#' @exportS3Method vctrs::vec_restore
vec_restore.age_group <- function( x, to, ...){
  # restore the age_group object
  ret <- new_age_group(start = field(x, "start"), end = field(x, "end"), symbols = get_symbols(to))
  return(ret)
}

#' @keywords internal
#' @exportS3Method ivs::iv_restore
iv_restore.age_group <- function(x, to, ...){
  ret <- new_age_group(start = field(x, "start"), end = field(x, "end"), symbols = get_symbols(to))
  return(ret)
}

# coercion functions ----------------------------------------------------------------------------------------------
#' @keywords internal
#' @export
vec_ptype2.age_group.age_group <- function(x, y, ...) new_age_group()
#' @keywords internal
#' @export
vec_ptype2.age_group.double <- function(x, y, ...) iv(double(), double())
#' @keywords internal
#' @export
vec_ptype2.double.age_group <- function(x, y, ...) iv(double(), double())

#vec_ptype_show(new_age_group(), iv(double(), double()), new_age_group())

#' @keywords internal
#' @export
vec_cast.ivs_iv.age_group <- function(x, to, ...){iv(as.double(iv_start(x)), as.double(iv_end(x)))}

#' @keywords internal
#' @export
vec_cast.age_group.ivs_iv <- function(x, to, ...){
  new_age_group(iv_start(x), iv_end(x), 
                symbols = list(below = "0-", sep = "-", above = "+"))
  }


# Function from iv age to character age groups ------
#' Age group into a character vector
#' 
#' This is the reverse of as.age_group().  Function arguments determine the format
#' of the decoded character age group. For the encoding scheme see [as.age_group()] details section.
#' 
#' @param groups is an iv vector 
#' @param sep defaults to " - ", but may be "to", "through", and "thru"
#' @param above defaults to " +" as suffix but may be set to " >" 
#' @param below defaults to "0" as in 0-4, can be "< " or "less than" for different way to express the 0 - age group.
#' @param all_ages defaults to "all", but may be set to "total"
#' @keywords internal
#' @export
age_group_to_char <- function(groups, sep = get_symbols(groups)[["separator"]] %||% "-", 
                            above = get_symbols(groups)[["above"]] %||% " +", 
                            below  = get_symbols(groups)[["below"]] %||% "0",
                            all_ages = "All") {

  sep = get_symbols(groups)[["separator"]] %||% "-"
  above = get_symbols(groups)[["above"]] %||% " +" 
  below  = get_symbols(groups)[["below"]] %||% "0"
  all_ages = "All"
  
  starts <- ivs::iv_start(groups)
  ends <- ivs::iv_end(groups)
  
  # format the regular character vector with separators for multiple ages covered and single numbers if only one age covered
  ret <- character(length = length(groups)) # ret is the vector to return
  
  # use a separator for multi-ages is TRUE
  multi_ages       <- (starts != (ends-1))
  ret[multi_ages]   <- paste0(starts[multi_ages], sep, ends[multi_ages]-1)
  
  # use a single age when separator is not needed
  ret[!multi_ages] <- starts[!multi_ages]
  
  # if any of the mutli age groups starts with zero and the '<' below character is present in symbols, then format the
  # group to use the below symbol and the end of the group to show less than that age.
  zero_present <-  (iv_start(groups) <= 0 & 0 < iv_end(groups)) & str_detect(below, "^<")
  any(zero_present)
  if(any(zero_present)) ret[zero_present] <- paste0(below, iv_end(groups[zero_present]))
  rm(zero_present)
  
  # check for using the above character when any group ends with Inf, then use the above symbol
  inf_present <- (iv_end(groups) == Inf)
  if(any(inf_present)) ret[inf_present] <- paste0(iv_start(groups[inf_present]), above)
  
  # finally, change any '0 +' to 'All'
  ret <- str_replace(ret, "^0 ?\\+", "All")
  ret
}


#' @param x  an age group object
#' @param sep a separator to use for the age groups.  Default is '-'.  Typically this will come from the age_group object.
#'
#' @return  age groups as a character vector.
#' @exportS3Method base::as.character age_group
as.character.age_group <- function(x, ...){
  syms <- get_symbols(x)
  if(! rlang::is_empty(x)) age_group_to_char(x, sep = syms$sep, above = syms$above, below = syms$below) else character(0)
}


#' @return a nominal factor of age groups.
#' @exportS3Method generics::as.factor age_group
#' @examples
#' 
#' ag <- as.age_group(c(0, 1, 5, 10, 20, 30, 40, 50))
#' ag 
#' ag_fac <- as.factor(ag)
#' ag_fac
#' 
#' ag_ord <- as.ordred(ag) 
#' ag_ord
#' 
as.factor.age_group <- function(x, ...){
    cls <- class(x)
    syms <- get_symbols(x)
    x <- sort(x) 
    class(x) <- cls
    attr(x, "symbols") <- syms
    x <- as.character.age_group(x)
  fac <- generics::as.factor(x)
  return(fac)
}

#' @return an ordinal factor of age groups.
#' @exportS3Method generics::as.ordered age_group
as.ordered.age_group <- function(x, ...){
  cls <- class(x)
  syms <- get_symbols(x)
  x <- sort(x) 
  class(x) <- cls
  attr(x, "symbols") <- syms
  x <- as.character.age_group(x)
  fac <- generics::as.ordered(x)
  return(fac)
}


#' @exportS3Method base::as.integer age_group
as.integer.age_group <- function(x, ...){
  ret <- iv_start(x) |> trunc()
  if(is.double(ret)) ret <- as.integer(ret)
  ret
}



# Functions used to create iv age groups and categorize  ---------------------------------------------------------

# internal convenience function to get the "problems" attribute character when checking for valid symbols
get_problems  <- purrr::attr_getter("problems")
# internal function to get the locations of invalid entries in character string of age groups.
get_locations <- purrr::attr_getter(("locations"))

#' Check for Valid Symbols
#' 
#' For a character vector representing age groups, are all of the non-digit symbols valid for an age_group object.
#' Certain symbols are considered valid for the below, sep, and above symbols
#'
#' @param x a character vector representing age groups, like "< 5", "1-4", "85 +"
#'
#' @return TRUE/FALSE
#' @export
#' @noRd
#' @examples
#' #todo
valid_age_group_symbol <- function(x){
  symbol_regex <-  regex("(through|thru|total|to|-|<|>|\\+|all|plus|above|,)", ignore_case = TRUE)
  invalid <- stringr::str_remove_all(x, symbol_regex) |> stringr::str_trim()
  malformed <- !(str_detect(x, symbol_regex) | str_detect(x, "^[:digit:]{1,3}$")) | str_detect(invalid, "[:alpha:]")
  
  if(any(malformed)){
    prob_ndx <- which(malformed)
    
      #paste(collapse = ", ")
    ret <- FALSE
    attr(ret, "problems") <- x[prob_ndx]
    attr(ret, "locations") <- prob_ndx
  } else {
    ret <- TRUE
    attr(ret, "problems") <- NULL
    attr(ret, "locations") <- NULL
  }
  return(ret)
}



#' Age groups class
#'
#' Convert integer, double,  character or factor vectors to a "age_group" vector. The "age_group" class represents
#' age ranges as an numeric interval. The is.age_group() function tests whether a variable is an "age_group" vector.  
#' 
#' @param x for as.age_group() a character, factor, numeric vector or interval vector [ivs::iv()]. Character based age
#'   groups,  like " <5, 5-10, 11-15", must have two two numbers separated by: "-", "to", "thru", or, "through". If x is
#'   numeric vector, each entry is treated as a cut point with each group abutting the previous group. See details
#'   below. For the is.age_group() function, x is an object to test.
#'   
#' @param below,sep,above  are scalar strings of valid symbols for age groups. Valid symbols are:
#'  * below: '<'
#'  * sep:   'through , 'thru', 'to', '-'
#'  * above: '>', '+', 'plus' 
#'  
#' @details 
#' The age_group class is based on numeric intervals, specifically the ivs class. An [introduction to
#' ivs](https://davisvaughan.github.io/ivs/articles/ivs.html) intervals explains what they are, notation, and the math
#' that can be done with them. Regarding notation,age groups are presented as "5-10"  to represent all ages beginning
#' with 5 and ending with 10. #' However, the underneath encoding is a right open interval in this case
#' "[5,11)" which means including five and all numbers through 10 but not including 11 which is the typical way ages are
#' interpreted.
#' 
#' To convert from a character or factor vector to age_group, the strings must be formatted using specific symbols to
#' show the age ranges. Those symbols are '-', ':', to',  'through', or 'thru'. The exception are single ages that are
#' made into an age group of one year.  The as.age_group() function also recognizes the prefix '<' to create a range of
#' ages from 0 up to but not including the given age. For example '< 5' would be all numbers below 5 but not including
#' 5. The Ages grouped above an age are recognized by the use of suffixes; '+', 'plus', 'above', and '>'.  These mean
#' the age stated plus all ages greater are included in the group. So, '80+' would be 80 and all greater ages. Two
#' special key words in the character vector can be used: 'all' and 'total', which are encoded as \[0,Inf) and means all
#' ages.
#'
#' A numeric vector will be converted to age groups by using each value as a cut point resulting in age groups with no
#' gaps. For example, a c(0,10,20,60,Inf) vector results in age groups: "0-9" "10-19", "20-59" and "60+". 
#' 
#' All age groups  are encoded as right open intervals .  Examples  "< 5", "0-4", "85+", "14" would be encoded as 
#' \[0, 5), \[0,5), \[85, Inf) and \[14,15) respectively. Note that a single year of "14" is an age
#' group of one year encoded as an interval '\[14,15)' and is interpreted as all values beginning with 14 and
#' up to but not including 15.
#'  
#' @return as.age_group() returns an "age_group" vector with attributes of symbols used for the below, sep, and above
#'   characters.
#'   
#' @examples
#' 
#' # character vector of age groups
#' char_age_groups <- c("0-5", "6-10", "11-20", "20+")
#' 
#' ag <- as.age_group(char_age_groups)
#' ag
#' attributes(ag)
#' 
#' # numeric vector of age cut points with 80 plus grouped together
#' num_cuts <- c(0,10,20,40,50,60,70,80,Inf)
#' ag_num <- as.age_group(num_cuts, sep = " to ", above = " plus")
#' ag_num
#' 
#' 
#' @export
as.age_group <- function(x, below = "0-", sep = "-", above = "+") { 
  UseMethod("as.age_group")
}

#' @rdname as.age_group
#' @exportS3Method as.age_group factor
as.age_group.factor <- function(x, below = "0-", sep = "-", above = "+") {
  as.character(x) |> 
    as.age_group.character()
}

#' @rdname as.age_group  
#' @exportS3Method as.age_group character
as.age_group.character <- function(x, below = "0-", sep = "-", above = "+") {
  #if(is.factor(x))  x <- as.character(x)
  # if(! is.character(x)){
  #   stop("groups is not a factor or character vector")
  # }
  
  x <- str_trim(x, side = "both") %>% str_to_lower()
  
  # check that the correct symbols are present
  valid_syms <- valid_age_group_symbol(x)
  if( isFALSE(valid_syms)){
    msg <- paste0('These age groups in x are malformed: "', get_problems(valid_syms), '"')
    stop(msg)
  }
  rm(valid_syms)
  
  symbols <- list(below     = character(),
                  separator = character(),
                  above     = character())
  
  # get the symbols or character strings used in formatting the age group, always chooe the first symbol encountered
  get_symbol <- compose(
    ~ if(length(.)) .[1] else NULL,
    na.omit, 
    ~ stringr::str_extract(string = x, pattern = .x)
  )
  
  below_str <- "^(< ?|0 ?-?)"
  symbols$below     <- get_symbol(pattern = below_str) %||% below
  
  separator_str <- " ?(-|to|thru|through) ?"
  symbols$separator     <- get_symbol(separator_str) %||% sep
  
  above_str      <-  " ?(plus|\\+|>|above)"
  symbols$above     <- get_symbol(above_str) %||% above
  
  # get starts and ends.  If special symbols/strings are present then replace them
  starts <- str_extract(x, regex("^(<|\\d{1,7}|all|total)", ignore_case = TRUE))   |>
    str_replace(pattern = regex("(all|total|<|-)", ignore_case = TRUE), replacement = "0") |>  # zero boundary
    as.double()
  ends   <- str_extract(x, regex("(>|\\d{1,7}|all( ages)?|total|\\+|above)$", ignore_case = TRUE)) |>
    str_replace(pattern = regex("(all( ages)?|total|>|\\+|above)", ignore_case = TRUE), replacement = "Inf") |>
    as.double()
  
  # check if any NAs exists and throw error with message
  st_ndx <- is.na(starts)
  en_ndx <- is.na(ends)
  if( any( st_ndx | en_ndx)) {
    ndx <- c(which(st_ndx), which(en_ndx)) |> unique() |> sort()
    err_age_groups <- x[ndx] |> paste(collapse = ", ") |> unique()
    msg <- paste0("Unrecognized groups in x: ", err_age_groups)
    stop (msg)
  }
  
  ends[! str_detect(x, "^<")] <- ends[! str_detect(x, "^<")] + 1
  ret <- new_age_group(starts, ends, symbols)
  
  return(ret)
}

# integer vectors cannot include the grouped upper age range as integer cannot include 'Inf'
# this function passes the vector to the as.age_group.double() function.
#' @rdname as.age_group
#' @exportS3Method as.age_group integer
as.age_group.integer <- function(x, below = "0-", sep = "-", above = "+") {#, inclusive = c("both", "below", "above", "none"), ...){ 
  x <- as.double(x)
  as.age_group.double(x, below, sep, above)
}

#' @rdname as.age_group
#' @exportS3Method as.age_group double
as.age_group.double <- function(x, below = "0-", sep = "-", above = "+") {#, inclusive = c("both", "below", "above", "none"), ...){ 
  assert_that( is.double(x))
  assert_that( min(x, na.rm = TRUE) >= 0)
  
  start <- x[1:length(x)]
  end <- c(x[2:length(x)], x[length(x)]+1)
  
  if(start[1] != 0) below = paste0(start[1], "-")   # age groups does not include 0
  
  
  # if Inf is present at the end, then trim both start and end
  if(end[length(end)] == Inf){
    start <- start[seq_len(length(x)-1)]
    end   <- end[seq_len(length(x)-1)]
  }
  
  syms <- list("below" = below, "sep" = sep, "above" = above)
  
  #check the symbols are valid
  valid_syms <- valid_age_group_symbol(unlist(syms))
  if( isFALSE(valid_syms)){
    msg <- paste0('These age groups in x are malformed: "', get_problems(valid_syms), '"')
    stop(msg)
  }
  rm(valid_syms)
  
  new_age_group(start = start, end = end, symbols = syms)
  #as.age_group.integer(as.integer(x), below, sep, above)
}

#' @rdname as.age_group
#' @exportS3Method as.age_group ivs_iv
as.age_group.ivs_iv <- function(x, below = "0-", sep = "-", above = "+") {#, inclusive = c("both", "below", "above", "none"),...){ 
  syms <- list("below" = below, "sep" = sep, "above" = above)
  x <- na.omit(x)
  new_age_group(ivs::iv_start(x), ivs::iv_end(x), syms) 
}

#' Categorize age to age group
#' 
#' Uses [ivs::iv] to categorize into interval age groups 
#' @param age_group a integer, or numeric vector that can be represented as age groups.  Integer and numeric 
#' vectors are internally converted to intervals as an age_group vector.
#' @param age a integer or numeric 
#'
#' @return  an ordered factor with where each ach is in age_group.
#' @keywords internal
#' @export
#'
#' @examples
#' 
#' # categorize each age into a age group
#' ages <- c(0L, 82L, 1L, 1L, 93L, 83L, 76L, 57L, 59L, 67L, 0L, 64L, 60L, 0L, 0L, 0L, 46L, 47L, 49L, 
#'          63L, 64L, 64L, 62L, 28L, 81L, 66L, 0L, 51L, 2L, 60L, 16L, 41L, 27L, 72L, 65L, 66L, 24L, 
#'          35L, 50L, 37L, 38L, 84L, 62L, 59L, 0L, 61L, 28L, 70L, 17L, 23L, 81L, 31L, 2L, 57L, 85L, 
#'          52L, 80L, 59L, 80L, 70L, 1L, 39L, 92L, 92L, 7L, 75L, 61L, 52L, 4L, 33L, 69L, 56L, 63L, 
#'          93L, 67L, 62L, 61L, 53L, 70L, 76L, 72L, 80L, 48L, 59L, 76L, 89L, 1L, 40L, 1L, 27L, 62L, 
#'          11L, 66L, 9L, 73L, 42L, 0L, 10L, 4L, 66L, 71L, 25L, 3L, 6L, 24L, 28L, 31L, 49L, 40L, 5L, 
#'          53L, 61L, 68L, 3L, 71L, 13L, 61L, 68L, 8L, 39L, 45L, 58L, 57L, 20L, 81L, 1L, 52L, 39L, 46L, 
#'          40L, 45L, 2L, 51L, 65L, 70L, 64L, 62L, 48L, 60L, 0L, 24L, 71L, 40L, 60L, 42L, 37L, 58L, 
#'          20L, 4L, 16L, 0L, 75L, 41L, 67L, 62L, 28L, 1L, 6L, 66L, 4L, 35L, 75L, 58L, 47L, 78L, 64L, 
#'          74L, 60L, 92L, 85L, 89L, 0L, 39L, 67L, 58L, 61L, 65L, 34L, 45L, 63L, 31L, 76L, 51L, 10L, 
#'          41L, 1L, 4L, 71L, 99L, 54L, 1L, 54L, 52L, 53L, 70L, 52L, 25L, 89L, 100L, 40L)
#'          
#'  groups <-   tarr::age_cat_iv(age = ages, age_group = tarr::age.groups$Yr.10)         
#'  head(groups, 12)
#' 
age_cat_iv <- function(age, age_group){
  if(is.numeric(age_group)) age_group <- as.age_group(age_group) |> sort()
  if(is.numeric(age)) age <- as.age_goup(age)
  
  # using interval logic find the location for each age in the age group.  The "haystack" vector is the integer of the 
  # factor, then assigning the lables from the age group as a factor.
  locations <- iv_locate_overlaps(needles = age, haystack = age_group, type = "within") |> 
    na.omit()
  res <- factor(x = locations$haystack, 
                levels = unique(locations$haystack), 
                labels = age_group,
                ordered = TRUE)
  return(res)
  
  # ret <- iv_align(needles = age, haystack = age_group, locations = locations)
  # if(nrow(locations) != length(age)) {
  #   warning('The following ages did not fit into an age group: "',
  #           paste(age[attr(locations, "na.action")], collapse = ", "), 
  #           '"')
  # }
  # ret[["haystack"]]
}

#' @returns is.age_group returns either TRUE or FALSE
#' @rdname as.age_group
#'
#' @export
is.age_group <- function(x){ inherits(x, "age_group")}


