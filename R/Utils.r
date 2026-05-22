# ===========================================================================
# Utils.r
# Useful utility functions
# ===========================================================================


# bar_wrap ---------------------------------------------------------------------
#' Wrap another function to show a progress bar
#'
#' This function wraps a function so that each time it is called a progress bar is shown and incremented. The purpose is
#' for user feedback when a function must be called many times such as in a loop, apply, or map function. This is
#' basically a function factory that returns a function that will show a progress bar and increment it each time it is
#' called. The progress package is used for the progress bar ([progress::progress_bar]).
#' 
#' @param .bar_format the format of the progress bar.  See [progress::progress_bar]
#' @param .fun is the function to be wrapped.
#' @param .total the expected total number of steps.
#' @param .step the number of times .fun is called for updating the progress bar.
#'   The default is 1
#' @param .msg a message to display above the progress bar.
#' @seealso [progress::progress_bar]
#' @return the wrapped function.  
#' @examples  
#' /dontrun{
#'  # List the csv files to read
#'  csv_files <- list.files(pattern = "csv$")
#'  
#'  # wrap read.csv to use a progress bar
#'  read_csv_bar <- bar_wrap(.fun = read.csv, 
#'                           .total = length(csv_files))
#'  # read in the csv files to a list of data frames.  A progress bar is shown
#'  in the console.
#'  df_list <- map(csv_files, read_csv_bar)  
#' }
#' @export
bar_wrap <- function(.bar_format = "(:spin)[:bar] :current eta: :eta",
                     .fun = NULL,
                     .total,
                     .step = 1,
                     .msg = "") {
  stopifnot(!is.null(.fun))
  pb <- progress::progress_bar$new(
    complete = ":",
    incomplete = "-",
    format = .bar_format,
    total = .total
  )
  current_tick <- 0
  msg_printed <- FALSE
  
  wrapped <- function(...) {
    if (length(.msg) > 0 & !msg_printed) {
      msg_printed <<- TRUE
      pb$message(.msg)
    }
    # increment the progress bar 
    current_tick <<- current_tick + 1
    if (current_tick %% .step == 0) {
      pb$update(current_tick / .total)
    }

    ret <- .fun(...)
  }
  return(wrapped)
}


# Function to create a comparison key -------------------------------------
#' Create a key field 
#' 
#' Takes passed vector variables for first name, last name and date of birth, to
#' make a "key."  This function is meant to be used in matching records in different
#' data frames using several variables to uniquely identify a record.  Character
#' variables are truncated to length. Date variables are converted to character but
#' not truncated. Only alpha-numeric characters are used, meaning all punctuation
#' symbols are removed e.g., "!,.'\. The returned key is an upper case concatenation
#' of the variables passed.
#'
#' @param len is the length  to use for character variables
#' @param fname is the first word/name to use
#' @param lname is the last word/name to use
#' @param dob a date, number or character string of numbers 
#' @param ...  a list of  variables (not yet implemented)
#' @return a character vector with the passed vectors concatenated together
#' @keywords internal
#' @export
create_key <- function(fname, lname, dob, len = 7){
  # convert dates to mm-dd-yyyy character format without formatting
  # this is done as the Jaro-Winkler string scoring method is sensitive to order
  # of dates and will show more dis-similarity is the year is last.
  
  #if(lubridate::is.Date(dob)) dob <- format(dob, "%m%d%Y")
  key <- create_key_v(fname, lname, dob, len = len)
  return(key)
}

#' Create a key field with several vectors
#'
#' Takes vector variables and makes a key.  This function is meant to be used in matching records in different data
#' frames using several variables to uniquely identify a record.  Character variables are truncated to length. Date, 
#' numeric and logical variables are converted to character but not truncated. Only alpha-numeric characters are
#' returned, meaning all punctuation  symbols are removed e.g., "!,.'\. The returned key is an upper case concatenation
#' of the vectors. Vectors accepted as arguments include character, Date, integer, numeric and logical.
#' @param ... vectors of character, Date, integer, or numeric.
#' @param len is the first characters of a character string to use in the character vectors. The default is 7.
#' @return the key with the passed vectors concatenated as character and upper case.
#' 
#' @export
create_key_v <- function(..., len = 7){
  vrs <- list(...)
  
  #check that all variables are the same length
  lens <- map_int(vrs, length)
  max_lens <- max(lens)
  all(lens == max_lens)
  stopifnot(identical(lens, rep(max_lens, length(lens))))
  rm(lens, max_lens)
  
  function_list <- list(
    "character" = purrr::compose(str_to_upper, ~stringr::str_sub(.x, 1, len), mash_name),
    "POSIXlt"   = mash_date,
    "POSIXct"   = mash_date,
    "Date"      = mash_date,
    "logical"   = as.character,
    "integer"   = as.character,
    "numeric"   = as.character
  )
  
  # check variables types are in the function list.
  acceptable <- map_lgl(vrs, ~ class(.x)[[1]] %in%  names(function_list))
  if(!all(acceptable)){
    err_msg <- paste("Variables must be integer, numeric, logical, Date, or character.
  Variable indices that are not:", paste(which(!acceptable), collapse = ", "))
    stop(err_msg)
  }
  rm(acceptable)
  
  
  #Use the function list to convert and format the list of variables
  chop_convert <- function(item){
    if(is.character(item)) item <- str_sub(item, 1, len)
    item <- function_list[class(item)][[1]](item)
    item
  }
  
  keys <- map(vrs, chop_convert) %>%
    reduce(., paste0) |> 
    str_to_upper()
  return(keys)
}



# only_pattern function factory ---------------------------------------
#' Retrieve specific characters
#' 
#' The only_pattern() function is a function factory that returns functions that will extract characters based on a 
#' regular expression or "pattern." Several "mash/only" functions that take a value and return only certain characters. 
#' are defined in terms of only_pattern(). As an example, only_digit() can take any sequence of characters and return
#' only the numerical characters or digits. only_digits("I am 25 years old") returns "25".  Included functions are
#' defined in terms of the only_pattern() function.
#' 
#' @details
#' These functions can be useful when you are trying to match vectors or keys.  For example if one
#' vector of phone numbers is formatted as "(817)321.4896", and another is formatted as "817/321-4986", the character
#' strings will be false when looking for equality (match) i.e., "(817)321.4896" != "817/321-4986". Using only_digit()
#' will cause them to match; only_digit( "(817)321.4896" ) == only_digit( "817/321-4986" ).
#'
#' These functions are mostly implemented by the function factory "only_pattern(pattern)". Where the pattern can be any
#' regex expression, see [regex()].  Functions that are define with only_pattern() are: 
#' * only_digit()/mash_phone() and only_alpha()/mash_name().
#' 
#' The only_date() function takes a vector of dates and returns a character of the dates without formatting characters 
#'
#'
#' @param pattern is any regular expression accepted by [stringr::str_extract_all]
#' @return only_pattern; a function that accepts a vector of character strings
#' @rdname Only.Functions
#' @examples 
#' only_an <- only_pattern("[:alnum:]")  # function to extract alphanumeric characters
#' only_an("743 Third street")
#' @export
only_pattern <- function(pattern) {
  ret <- function(string) {
    # remember that compose first function is the last one listed!
    compose(~purrr::map_chr(., ~stringr::str_c(.x, collapse = "")), 
            ~stringr::str_extract_all(string = ., pattern = pattern), 
            as.character)(string)
  }
  return(ret)
}


# mash_phone takes a phone number and removes non-numeric characters --------------
# this function can be used with dates as well. 
#' @param string a character string that may contain numeric characters
#' @return mash_phone and only_digit: a character string with only the numeric characters
#' @rdname Only.Functions
#' @export
#' @examples
#' 
#' # get only the digits from a string of characters
#' mash_phone("Force was 200 newtons")
#' 
#' # The same function as an alias name
#' only_digits("Force was only 5 newtons")
#' 
mash_phone <- only_pattern("[:digit:]")

#' @rdname Only.Functions
#' @export
only_digits <- mash_phone

# mash_name --------------------------------------------------------------------
#' @rdname Only.Functions
#' @return mash_name and only_alpha: return a character string of only alphabet charatcers.
#' @export
mash_name <- only_pattern("[:alpha:]")

#' @rdname Only.Functions   
#' @export
only_alpha <- mash_name


# mash_date --------------------------------------------------------------------
#' @param date is a vector of Dates
#' @return mash_date: a character vector with the dates formatted as mmddyyyy
#' @rdname Only.Functions
#' @export
mash_date <- function(date){
  format(date, format = "%m%d%Y")
}


iconv_part     <- purrr::partial(iconv, sub = "byte")
fix_multi_byte <- purrr::compose(stringr::str_to_upper,iconv_part, enc2utf8)


# %nin% ------------------------------------------------------------------------
#' Not %in%
#' 
#' This is a negation of the %in% operator and is synonymous to "! vector1 %in% vector2".
#' It is an infix operator, meaning it is placed between two arguments.
#' @param x a vector
#' @param table a vector of values to match against
#' @return a logical vector indicating if an element in x is  not found (TRUE) or found (FALSE) in table.
#' 
#' @examples
#' vec1 <- letters[1:10]
#' vec2 <- letters[8:25]
#' vec1 %nin% vec2
#' @export
`%nin%` <- purrr::negate(`%in%`)

#' Find minimum R version needed for a package
#'
#' @param package the name of a package to check on CRAN
#'
#' @noRd
find_transitive_minR <- function(package) {
  
  db <- tools::CRAN_package_db()
  
  recursive_deps <- tools::package_dependencies(
    package, 
    recursive = TRUE, 
    db = db
  )[[1]]
  
  # These code chunks are detailed below in the 'Minimum R dependencies in CRAN 
  # packages' section
  r_deps <- db |> 
    dplyr::filter(Package %in% recursive_deps) |> 
    # We exclude recommended pkgs as they're always shown as depending on R-devel
    dplyr::filter(is.na(Priority) | Priority != "recommended") |>  
    dplyr::pull(Depends) |> 
    strsplit(split = ",") |> 
    purrr::map(~ grep("^R ", .x, value = TRUE)) |> 
    unlist()
  
  r_vers <- trimws(gsub("^R \\(>=?\\s(.+)\\)", "\\1", r_deps))
  
  return(max(package_version(r_vers)))
}


#' Determine if vector is yes/no/unk 
#' 
#' Use is_yn() to check if the passed vector has values that can be termed "yes/no/unknown". If TRUE, the vector can be
#' converted to a logical vector. The yes values may be; yes, y, true, t, or 1.  The no values may be; no, n, false, f, or '0'.
#' The unknown values may be; unknown, unk, u, or na. Missing values, NA, may be present.
#' 
#' To be a yes/no/unk vector The number of unique values must be between 2, 3, or 4. Having 4 unique values is only
#' possible if the missing NA is one of the values.  The unique values in the vector should have no more than one 
#' value from the yes, no, and unknown categories.  The function is not sensitive to case.
#' 
#' 12/2/2025 Logical vectors return  FALSE.
#'
#' @param vec a character vector or factor.  If a factor then levels are used to check for yes/no/unknown values
#'
#' @return for is_yn(),  TRUE or FALSE.  If TRUE, the attribute 'value' is set to the yes/no/unknown unique values  
#' @export
#' @examples
#' # todo
is_yn <- function(vec){
  if(is.logical(vec)) return(FALSE)
  if( is.factor(vec)) vec <- levels(vec)
  if( ! is.character(vec)) vec <- as.character(vec)
  values <- unique(vec) |> str_to_lower() |> unique() |> na.omit()
  if(! between(length(values), 2,4)) {
    return(FALSE)
  }
  
  # find each code that can be used
  codes <- list(
    true_code  = c("t", "true", "y", "yes", "1"),
    false_code = c("n", "no", "f", "false", "0"),
    unk_code   = c("u", "unk", "unknown", "na", "")
  )  
  
  if(! all(values  %in% unlist(codes))) return(FALSE)
  
  value_search <- compose(
    \(lst) discard(.x = lst, .p = \(x) length(x) == 0),
    \(val) map(codes, \(cd) (val == cd) |> which())
  )
  
  atr <- map(values, value_search) |> unlist() |> 
    imap( \(x, nm) codes[[nm]][x]) |> unlist()
  
  
  if(length(atr) & any(names(atr) %in% c("true_code", "false_code")) ){
    ret <- structure(TRUE, values = atr)
    return(ret)
  }
  
  return(FALSE)
}

# show TRUE for all yn columns, FALSE otherwise
#' Identify yes no columns in a data frame
#' 
#' A convenience function to find data frame columns or list items that use yes/no values that may be converted to a
#' logical vector.
#' 
#'
#' @param df A data frame or list
#'
#' @return for id_yn_cols, a character vector of column names that were identified to be yes/no/unknown
#' @seealso [is_yn()]
#' @export
#' @rdname is_yn
#' @examples
#' #todo
id_yn_cols <- function(df) {
  lgl <- map_lgl(df, is_yn) #|>  
    #unlist() 
  names(df)[lgl]
}


# local function to handle Yes/No/Unknown into True/False/NA
#' Convert a yes/no/unknown vector to a logical vector
#' 
#' For yn_2_logical a vector that use "yes/y/t/true/1", "no/n/f/false/0", and "unknown/unk/na" values are converted to a
#' logical vector. The function first checks that it can be converted using [is_yn()].  Yes becomes TRUE, no becomes
#' FALSE, and unknowns are set to NA.
#'
#' @param vec a character vector or factor.  If a factor then levels are used to check for yes/no/unknown values
#'
#' @return for yn_2_logical, a logical vector.  If the vector does not meet the requirement for conversion, returns NULL with a warning.
#' @export
#' @rdname is_yn
#' @examples
#' # todo
yn_2_logical <- function(vec){
  yn <- is_yn(vec)
  
  if(! yn)  return(NULL)
  
  yes <- attr(yn, "values")["true_code"]
  no  <- attr(yn, "values")["false_code"]
  unk <- attr(yn, "values")["unk_code"]
  
  
  codes <- list(
    true  = c("(t|true|y|yes|1)"),
    false = c("(n|no|f|false|0)"),
    unk   = c("(u|unk|unknown|na)")
  )  
  
    
  vec <- str_to_lower(vec)
  ret <- logical(length = length(vec))  # fills in NA  to length
  ret[vec == yes]   <- TRUE
  ret[vec == no]    <- FALSE
  ret[is.na(vec) | vec == unk] <- NA  # this line may not be needed
  ret
}


# classifyCSV 
#
#' Determine the report origin for a csv file
#' 
#' Internally used function to classify a CSV file as coming from one of the report templates. The function examines the
#' passed data frame and classifies it as:
#'  0. Unknown
#'  1. Texas Flat file
#'  2. Line List without Jurisdiction security
#'  3. Line List with Jurisdiction security
#'  4. No Jurisdiction Disease Counts without security (same  as "Line List without Jurisdiction security")
#'  5. Houston Enhanced
#'  6. TriSano line list
#'  7. EpiTrax line list 
#'
#'  The function expects field names to use periods between words, usually this is done
#'  by using the read.csv function.  read_csv from  the readr package returns names with blanks
#'  so those names must be manipulated in order to use this function
#'  
#' @param df is a data frame from a  csv file.
#' @param return.name if FALSE (default) returns the index of reportNames of the matching report , TRUE
#'   returns the name of the report
#'
#' @return an integer of the type of file detected
#' @keywords internal
classifyCSV <- function(df, return.name = FALSE){
  stopifnot(is.data.frame(df))
  testNames <- names(df)
  
  # For reportNAmes$Jur, there are several calculated fields that do not exist in the example file.  Therefore
  # these fields are eliminated before checking if all the reportNames fileds exists in the passed df
  local_reports <- reportNames
  local_reports$Jur <- local_reports$Jur |> 
    select(-any_of( c("Event.Month", "Onset.MMWR.Week", "AgeYrs", "Investigation.ID.trisano", "Race.Eth") ))
  
  vec <- purrr::map_lgl(local_reports, function(x) all( names(x) %in% testNames))

  # Warning message if report does not match those in the reportNames list
  if(!any(vec)){
    warning("Unable to classify this report format.  \nDid you select the wrong file?")
    return(0)
  }
  
  if(return.name){
    ret <- names(local_reports)[which(vec)[1]]
  } else {
    ret <- which(vec)[1]
  }
  return(ret)  # return the first matched index, prevents duplicate indices being returned
}


# internalChooseCSV

# internally used function that opens a dialog box to read a csv or text file
# @param caption is the caption to use in the dialog box
#
# @return the selected file name or NULL if escape is pressed
internalChooseCSV <- function(caption = "Choose CSV file data to open or press \"Esc\" ",
                              filters = NULL){
  if(is.null(filters)){
    flt <- matrix(data=c("Comma separated values", "*.csv","Text", "*.txt"),
                  ncol=2, byrow=TRUE)  # set up the file filters to use when choosing the file
  } else {
    stopifnot(is.matrix(filters) & ncol(filters) == 2)
    flt = filters
  }
  
  if(Sys.info()[["sysname"]]=="Linux"){
    fn <- file.choose()
    #fn <- rChoiceDialogs::rchoose.files(caption = caption, multi = FALSE, filters=flt, index=1, default = paths$data)
  } else {
    winDialog(type = "ok", caption)
    fn <- choose.files(caption = caption,
                       multi = FALSE,
                       default = paste(paths$data, flt[1,2], sep = "/"),
                       #default = paste0(paths$data,"/*.csv"),
                       filters=flt,
                       index=1)
  }
  rm(flt)  # remove file type variables that are no longer needed
  return(fn)
}



#' Convert vector to compatible type
#'
#'  Simple function that converts the type of vec1 to that of vec2.  Can handle most base types including
#'  factors and ordered factor.  Note that for ordered factors it is an error if vec1 has values that 
#'  are not in vec2.
#'
#' @param vec1 The vector to convert 
#' @param vec2 The template vector
#'
#' @return vec1 converted to vec2 type
#' rdname Utility
#' @export
#'
#' @examples
#' # vector of dates but are actually character strings. 
#' v1 <- c("1999-09-01", "2022-10-29", "2005-06-25")
#' class(v1)
#' 
#' # vector of dates 
#' v2 <- seq(as.Date("1995-05-12"), as.Date("2015-03-1997"), by= "day")
#' class(v2)
#' 
#' # convert 
#' v3 <- convert_type(v1, v2)
#' v3
#' class(v3)
#' 
convert_type <- function(vec1, vec2){

    # converts a vector to an ordered vector 
  as.ordered_convert <- function(vec, template = vec2){
    orig_levels <- unique(vec)
    template_levels <- levels(template)
    delta <- setdiff(orig_levels, template_levels)
    if(length(delta)) {
      parent_name <- as.character(sys.call(-1)[[1]])
      cli::cli_abort(message = c("Error in {parent_name}(vec1, vec2) converting to ordered factor",
                                 i = "{length(delta)} values in vec1 are not present in vec2",
                                 x = "{head(delta, 7)}")
      )
    }
    ordered(vec, levels = levels(template))
  }
  
  funcs <- list(
    "integer"         = as.integer,
    "numeric"         = as.numeric,
    "Date"            = as.Date,
    "POSIXct"         = as.POSIXct,
    "character"       = as.character,
    "logical"         = as.logical,
    "factor"          = as.factor,
    "ordered"         = as.ordered_convert)
  
  sel <- class(vec2)[1]  # class of date time is "POSIXct and POSIXlt", use the the first one "POSIXct"
  funcs[[sel]](vec1)
  #ret <- if(sel == "ordered") as.ordered_convert(vec1, vec2) else funcs[[sel]](vec1)
}


# Messages shown only during interactive sessions ----------------------------------------------------------------
# internal simple function that will display a message only if the session is interactive
interactive_msg <- function(msg = NULL){
  if( ! is.null(msg) & interactive()) cat(msg, "\n")
  invisible()
}


# Backup file function --------------------------------------------------------------------------------------------


#' Create a backup file
#' 
#' Internal function creates a file backup using today's date and time stamp. The backed up file 
#' will have the same extension as the original file
#'
#' @param file the path and file name to be backed up
#' @param folder an optional folder where the backed up file will be located.  The default is to place the 
#' back up file in the same folder where the original file is found.  
#'
#' @return invisibly returns a logical, TRUE for success and FALSE for failure
#' @noRd
backup <- function(file, folder = dirname(file)){
  if(is.null(folder) || ! file.exists(folder))  stop("Could not find the folder to place the back up file", folder)
  if(! file.exists(folder)) stop("The folder ", folder, "does not exist")
  if(! file.exists(file))   stop("The ", basename(file), " file was not found")
  
  # construct the new path and file name 
  root <- tools::file_path_sans_ext(basename(file)) # the file name without path and extension
  ext  <- tools::file_ext(file)  # get the file extension
  dt   <- lubridate::now() |> format("%b-%d-%y_%H-%M-%S")  # construct the date time stamp
  
  # put the path and new ile name together
  bkup_name <- paste0(root, "_", dt, "_backup.", ext)
  newName <- file.path(folder, bkup_name)
  rm(root, ext, dt, bkup_name)
  
  
  if(file.rename(from = file,to = newName)){
    interactive_msg(paste("The original file", basename(file), "has been renamed ", basename(newName), " in folder\n", 
                    folder))
    return(TRUE)
  }
    
  interactive_msg(paste(basename(file), "could not be backed up"))
  return(FALSE)
  
}

# Check for missing data in required fields -------------------------------------------------------------
#' Identify rows with missing required data
#' 
#' Checks selected columns in a data frame that have missing ("" or NA) data.  Character columns are checked for blanks
#' as well as NA.  All other types are just checked for NA.
#'
#' @param df a data frame
#' @param cols vector of unquoted column names to check for missing data.
#' @param each_col When TRUE (default) each column is checked separately.  For example if two columns are passed each is checked 
#' and the returned row numbers represents where **any** columns have missing data.   When this is false, then row numbers 
#' are returned where **all** columns have missing data for that record.
#'
#' @return row numbers of records where the passed columns are missing data
#' @export
#'
#' @examples
#' #TO DO
check_col_NA <- function(df, cols, each_col = TRUE){
  ret <- df |> 
    select( {{cols}} ) |> 
    mutate(across(everything(), is.na)) |> 
    rowwise() |> 
    mutate(test_col = if(each_col) {
      any(pick( {{cols}} )) 
    } else  {
      all( pick( {{cols}} )) 
    }
    )
  
  which(ret$test_col)
}


#' Replace Strings in a Character Vector
#'
#' Character string elements or "terms" can possibly refer to the same entity, like a disease condition, and can often
#' be replaced with simpler terms. This function was originally written to simplify and collapse older NBS
#' condition names for the monthly report. For example, West Nile non-neuroinvasive disease has had several names in 
#' NBS resulting in multiple conditions showing up as different rows when counting cases.  However, they are the the
#' same condition. This function with the default [simple_conditions] named vector formats the condition names to a
#' common and more simple name. You can expand the use of simple_vectors by using the [c()] function to add additional
#' search and replace named characters for the simple_term argument. The names of the simple term named character vector
#' are treated as regular expressions ignoring the character case by default, and the value is the replacement.
#' 
#' @param old_term a character vector containing terms to be replaced.
#' @param simple_term a named character vector with names used as a regular expression to find and replace with the
#'   value of the vector. Defaults to the package supplied "simple_conditions" named character vector.
#' @param ignore_case by default case is ignored.  Set to FALSE to use case matching.
#' @returns a character vector with terms replaced
#' @export
#'
#' @examples
#' test <- c("West Nile Fever", "West Nile Virus, Non-neuroinvasive", "West Nile Virus Non-neuroinvasive disease")
#' simplify_term(old_term = test)
#' 
simplify_term <- function(old_term, simple_term = simple_conditions, ignore_case = TRUE){
  assert_that(length(names(simple_term)) > 1)
  assert_that(length(names(simple_term)) == length(simple_term)) 
  
  # Note str_replace_all almost works for this purpose, however it does not replace the whole element.
  unique_terms <- old_term |> unique()
  new_terms <- unique_terms
  iwalk(simple_term, \(x, nm) new_terms[str_detect(new_terms, nm)] <<- x)
  lookup <- set_names(new_terms, unique_terms)
  return(lookup[old_term] |> unname())
}
  
