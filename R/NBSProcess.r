#==============================================================================================================>
# NBSProcess.r
# Based on "NBS Case Line List Processing ver 3. r"
# Processes a case line list as downloaded from NBS.
# The NBS reports that this function can handle are:
#  - "Line List of Individual Cases with Program Area and Jurisdiction Security"
#  - "Line List of Pertussis Cases with NBS Security"
#  - "Texas Flat" when "Line List of Individual Cases with Program Area - NO
#     Jurisdiction Security" is present
#
# The resulting file from this script is saved as base.rdata in the data folder
#
# Modified June 1, 2016
# Changed the script to load a "base.rdata" file that contains previous
# processed NBS report downloads Prompts the user for the new records and
# processes those records, then combines the base and new records Deduplicates
# the new combined file and saves it as the new "base.rdata"
#
# Modified March 10, 2017.  If no file is chosen to import, then returns 'dis'
# as is from base.rdata this allows the user to get the base data quickly.  In
# other words press 'Esc' and get the dis data frame.
#
# Modified June 28, 2017.  Changed path for reading and saving data to be on the
# shared drive
#
# Modified July 10, 2017 Sources "paths.r" to set up data and script folder
# locations.  If changing locations of these in the future it can be done in one
# place - the paths.r file

# Modified June 2, 2018 The age category function (age.cat) is now in a separate
# file and has additional capabilities. AgeYrs calculation re-written to use
# Event.Date when OnSet.Date is missing, and to use reported age when the
# Birth.time is missing. The base.rdata file was updated to reflect the revised
# AgeYrs calculation.

# Modified August 22-24, 2018
# Moved to the tarr package and made into function nbsProcess.  All library
# commands removed and use references instead Added documentation
#
# Modified October 5, 2018
# The NBS line list report has been non-functional. However the line list with
# no jurisdiction security has remained available.  While several important
# fields for geocoding are missing from the no jurisdiction security report,
# general records are still available for counts and rates. The ability to read
# the still functioning line list was begun.  The module will eventually check
# for the no-security file type, adds blank fields, remove the few extra fields
# and adds it to the base rdata file. A further improvement to be implemented is
# checking for a valid with security file and replacing the
# reords with missing data.
#
# Modified October 28, 2018
# Added retrieve_base_data function.
#
# Modified December 27, 2018
# Added several internal functions and re-wrote nbsCSVRead to handle putting
# together two NBS reports that simulate the regular line list report.  This was
# done to work around NBS report failures since October 2018
#
# July-September 2019 Multiple changes to incorporate the TriSano report into
# the base.rdata file. Renamed some functions as NBS reports are no longer the
# sole target.  Old names for functions are maintained for backward
# compatibility.
#
# October - November 2019
# Added named vectors to assist in translating from TriSano to NBS including:
# field_names and disease_map
# 
# Sept-October 2024
# Using parquet files  instead of rds files.   The trisano function are being removed.
# EpiTrax functions have been updated and moved to a seprate file
#
#==============================================================================================================>

#' Case Line List Data Processing.
#'
#' This function is deprecated.  It now passes the argument onto to [baseData()].
#'
#' @param .base is the file name that contains the previously processed records as a data frame. the default is to load
#'   "base.rdata" from paths$data.  if 'Esc" is pressed, then just loads and returns the data frame in .base without
#'   further processing.
#' @param .chooseNew controls whether the function asks the user to select a csv file with addtional records.  The
#'   default is TRUE.  If FALSE, then the current .base file is loaded and returned.
#' @return returns a dataframe with the base data
#' @keywords internal
#' @export
baseNbsProcess <- function(.base = "base.rdata", .chooseNew = TRUE){
  interactive_msg("baseNBSProcess is deprecated.  \nSubstitute the baseData function in code\n Running baseData")
  baseData(.base, .chooseNew)
}


#' Case Line List Data Processing.
#'
#'  Get the base disease data.  When '.choosNew' is FALSE the current base data data frame containing the previous
#'  years' NBS data is returned. When '.choosNew' is TRUE, the function will request the user to select a csv file that
#'  has been sourced from the EpiTrax "Epi Monthly Report - Version 2" report.  For the EpiTrax exported file, it is
#'  usually best to to ensure the records exported are from the first of the year until current date. See details below
#'  for the updated process.
#' 
#' Updated April 2023 the current process logic is to load the previous years base NBS data and use the current  EpiTrax
#' data for disease reports. This keeps previous year counts in sync with DSHS. Updating the base data with new NBS records
#' is done in [baseNBSUpdate()] function.
#'
#' @param .base is the file name that contains the previously processed records as a data frame. The default is to load
#'   "base.parquet" from paths$data/NBS/nbs_base.parquet.  if 'Esc" is pressed, then just loads and returns the data
#'   frame in .base without further processing.
#' @param .chooseNew controls whether the function asks the user to select a csv file with additional records from
#'   EpiTrax. The default is TRUE.  If FALSE, then the current .base file that contains previous years of NBS data is
#'   loaded and returned.
#' @return returns a data frame with the base NBS data plus the optional EpiTrax data.
#' @export
#'
#' @examples
#' \dontrun{
#' disRecords <- baseData(.chooseNew = FALSE)  # returns all previously processed cases
#' newRecords <- baseData(.chooseNew = TRUE)   # prompts for EpiTrax csv file to incorporate.
#' }
baseData <- function(.base = file.path(paths$communicable, "NBS/nbs_base.parquet"), .chooseNew = TRUE){
  
  # ---- Load the base data file
  dis <- retrieve_base_data(.base) |> 
    filter(! duplicated(Investigation.ID))
  
  stopifnot(exists("dis"))

  # get the folder where .base is found
  pth <-  dirname(.base)

  # --- get new data from EpiTrax
  if(.chooseNew){
  
  # select the epiTrax data and import, de-duplicate epitrax records
    epitrac_recs <- epitrax_import() 
    if(nrow(epitrac_recs) == 0) {interactive_msg("Returning the base NBS data only\n"); return(dis)}
    epitrac_recs <- epitrax_2_nbs_format(epitrac_recs)
    epitrac_recs <- epitrac_recs |> 
      distinct(Investigation.ID, .keep_all = TRUE) |> 
      distinct(Investigation.ID.trisano,  .keep_all = TRUE)
    
    # select the field names in common
    dis_flds     <- names(dis)
    epitrax_flds <- names(epitrac_recs)
    common_flds  <- dis_flds[dis_flds  %in% epitrax_flds]
    
    dis     <- dis[, common_flds]
    epitrac_recs <- epitrac_recs[, common_flds]
    
    if(!is.null(epitrac_recs)){
      epitrac_recs <- epitrac_recs %>%
        mutate_if(is.factor, .funs = as.character) %>%
        as.data.frame()
      
      all_recs <- bind_rows(epitrac_recs, dis, .id = "set_num") |> 
        mutate(dataset = case_when(
          set_num == 1 ~ "Epitrax",
          set_num == 2 ~ "NBS",
          .default = "UNKNOWN"
        )) |> select(- set_num) |> 
        distinct(Person.Name, Birth.Time, Condition, Event.Date, .keep_all = TRUE)
      return(all_recs)
      
    }
  } else return(dis)
    
    
}

# Select and Process Case Data Line List from the NBS and TriSano.
#
# Asks the user to select a csv file  resulting from the NBS "Line List With Jurisidction Security" and the TriSano
# "Line list" reports. Reads and processes the files including:  converting some variables to factors, dates,
# calculating age groups, re-categorising race/ethinicity. This function does save a copy of the processed data frame
# as a compressed rdata file with the same filename as the respective csv file but changes the extension to "rdata".
# This is a caching scheme so that if the same csv file is chosen a second time, the function re-loads the previously
# processed file.
#
# December 27, 2018
# Added capability to read the NBS line list without jurisdiction security. Missing fields from this line list report
# are added, but have NA entered for the missing values. The important missing data includes date of birth, address and
# hospital information.
#
#  If  'Esc' is pressed when choosing the csv file then the function returns
#  NULL.
#
# July 26. 2019
# Added reading of TriSano line list report and merging with the NBS report
#
# September 2019
# Made the reading or formatting of the data files more robust. Deduplication is now done on "Investigation.ID",
# "Trisano.Investigation.ID" and demographic data. This was done as all cases in TriSano should eventually be listed in
# the NBS report as well.  NOTE that deduplication code needs further development.  As of 9/19/19 The demographic data
# is the person's name, birth date, condition, and event date.  It is possible that a person could be counted twice
# when the event dates are within 6 months of each event.
# 
# April 2023, using epitrax_import function instead of trisano_import updated fields that are in common between epitrax
# and NBS reports
#
#@return a data frame with the processed records from the selected csv file.
#  Records that have missing data in Person.Name or Case.Status, and those
#  other than "Tarrant County" are excluded.
# @export
# nbsCSVRead <- function(){
# 
#   nbs_recs <- nbs_import()
#   if(is.null(nbs_recs)){
#     return(NULL)
#   }
# 
#   # ---- section to ID duplicate investigations within 6 months of each other
#   nbs_recs <- nbs_recs |> 
#     distinct(Person.Name, Condition, Birth.Time, Event.Month, .keep_all = TRUE)
# 
#   nbs_recs <- nbs_recs %>%
#     # ---- Age Groups using predefined age groupings
#     dplyr::mutate(Age.Grp5  = age.cat(.ages = AgeYrs,.by= age.groups$Yr.5),
#                   Age.Grp10 = age.cat(AgeYrs,.by=age.groups$Yr.10),
#                   Age.Grp   = age.cat(.ages = AgeYrs,.by = age.groups$IMM)) %>%
#     # ---- Calculate a cleaned address
#     #dplyr::mutate(Address   = makeAddresss(.)) %>%
#     #arrange(Investigation.ID) %>%
#     # ---- Deduplicate based on Investigation IDs, does not remove duplicates from NBS/TriSano
#     #filter(!duplicated(dplyr::coalesce(Investigation.ID, Investigation.ID.trisano))) %>%
#     # ---- Deduplicate based on name, birth date, condition, and Events
#     #filter(!duplicated(paste(Person.Name, Condition, Birth.Time, Event.Date))) %>%
#     filter(!is.na(Case.Status))
# 
#   return(nbs_recs)
# }


#  nbs_import
#  Used  by CSVRead to:
#' Import an NBS  report.
#'
#' Imports an "NBS line list with jurisdiction security" report saved as a csv file. If no file name is provided to
#' read, then the function prompts the user to select a csv file via an interactive file browser. The [nbs_process()]
#' function is called to format and returns the finished data frame. Only selected fields from the CSV file are used.  
#' A caching scheme is used so that if the csv file has been previously processed as evidenced by the same file name
#' with a "parquet" extension, it  function returns that previously processed data frame. You can change this behavior by 
#' setting .use_cache = FALSE.
#' 
#' By default only confirmed and probable cases are returned.  If all records are desired, then set .include.all to
#' TRUE.
#'
#' @param .file is the name of the file to import.  If NULL (default) will prompt the user to choose the file
#' @param .include.all when FALSE (default), just confirmed and probable cases are returned.  For TRUE all cases are, 
#' the .file argument is used to read in the csv file and all records are returned without regard to Case.Status
#' @param .use_cache when TRUE (default) will look for and return the contents of a parquet file with the same name as
#' what was passed in .file    Note that this is bypassed and the csv file is always read and processed when .include_all
#' is TRUE.
#' @return a data frame with selected fields. if 'Esc' is pressed returns NULL!
#' @export
#'
#' @examples 
#' \dontrun{
#' diseases <- nbs_import()
#' }
#'
#' @family Reading report files
#' @seealso 
#'   \code{\link{nbs_import}} for importing a NBS line list report, and
#'   \code{\link{epitrax_import}} for importing a TriSano line list report
#'
nbs_import <- function(.file = NULL, .include.all = FALSE, .use_cache = TRUE) {
  # .file = nbs_file
  # .include.all = TRUE
  # Select and read in the chosen file,  process each variable as needed
  if(is.null(.file)){
    fn <- internalChooseCSV(caption = "Choose the NBS report file",
      tarr.filters[c("NBS","csv"),]
    )
  } else {
    fn <- .file
    stopifnot(file.exists(fn))
  }

  # return early if Esc was pressed
  if(length(fn) ==0) {  # escape pressed?
    return(NULL)
  } else {

  # ---- Check if this CSV file has already been processed and load if .include.all is FALSE
  fname <- sub(pattern = "\\.csv$", replacement = ".parquet", fn, ignore.case = TRUE)
  if(file.exists(fname) & .include.all == FALSE & .use_cache == TRUE){
    interactive_msg(paste0("The NBS report file: ",basename(fname),
                " was previously processed. Cached data file has been loaded"))
    return(nanoparquet::read_parquet(fname))
  } else {  # not previously processed, read in csv file.
    stopifnot(!is.null(fn) & file.exists(fn))
    nbs <- read.csv(file = fn, header = TRUE, as.is = TRUE, stringsAsFactors = FALSE)
    if(str_detect(fn, "WithJurisExample2.csv")) {
      dis <- nbs_process(.df = nbs, .include.all = .include.all, .report_type = "Jur")
    } else {
      dis <- nbs_process(.df = nbs, .include.all = .include.all)
    }
    }
  }

  # Cache only the default confirmed/probable result. An all-status import
  # must not overwrite the cache used by default calls.
  if(!.include.all) write_parquet(dis, file = fname)
  #save(dis, file=fname)
  return(dis)
}

#' nbs_process
#'
#' Processes a NBS report, preferably the 'case line list with jurisdiction.' The data frame is processed including
#' calculation of variables, formatting and variable selection. Enforces the use of Investigation.ID2 as Investigation
#' ID. The processed data frame is returned.
#' 
#' @param .df is the data frame with the NBS report, including path to the file
#' @param .include.all when FALSE (default), just confirmed and probable cases are returned.  TRUE all cases are
#'   returned without regard to Case.Status
#' @param .report_type this is used internally when constructing the package. When set to a character name from 
#' reportNames it causes that report type to be used, bypassing the usual classifyCSV function.
#' @details Several transformations are made to the data:
#'   - Only Tarrant County cases are selected Race and ethnicity are recoded as one variable. 
#'   - Age in years is calculated from date of birth where data is available.
#'   - Several variables with short values are given long values (e.g., M,F,U to Male, Female, Unknown)
#'   - Variables with Yes/No/Unk are converted to logical (added 9/23/2024)
#'
#' @return  a dataframe with the processed NBS report informarion.  Error when a
#'   non-valid filename is passed
#' @export
#' @examples
#' \dontrun{
#' cases <- nbs_process(.df = NBS_dataframe)
#' }
#'
#' @family Reading report files
# seealso \code{\link{nbsCSVRead}} for reading the NBS and Trisano line lists
#   \code{\link{nbs_import}} for importing a NBS line list report
#   \code{\link{triSano_import}} for importing a TriSano line list report
nbs_process <- function(.df, .include.all = FALSE, .report_type = NULL) {
  # Classify the report type
  if(!is.null(.report_type )){
    NBSReportType = which(names(reportNames) %in% .report_type)
  } else{
    NBSReportType <- classifyCSV(.df)
  }
  
  if(!NBSReportType %in% c(1:4,6)){
    stop("Not a recognized NBS report")
  }
  if(NBSReportType == 1){   # Texas flat file report selected
    tcltk::tk_messageBox(type = "ok", caption = "One more CSV file to choose",
                         message = "Flat file selected!\nChoose the associated \"Line List without Jurisdiction security.csv\" ")
    noJurisFN <- internalChooseCSV("Line list CSV file without jurisdiction security ")
    if(length(noJurisFN) == 0){
      tcltk::tk_messageBox(type = "ok", caption = "Escape key pressed",
                           message = "Associated line list not found.  \nBase rdata file will be used.")
      return(NULL)
    }
    noJuris <- read.csv(noJurisFN, as.is = T, header = T)
    .df <-  constructDiseaseDataFrame(flat = .df, noJuris = noJuris)
    rm(noJurisFN, noJuris)

  } else if(NBSReportType == 2) {  # Line list without jurisdiction security chosen
    tcltk::tk_messageBox(type = "ok", caption = "One more CSV file to choose",
                         message = "Line list without jurisdiction securitychosen!\nChoose the associated \"Texas Flat file.csv\"")
    flatFn <- internalChooseCSV("Texas Flat CSV file")
    if(length(flatFn) == 0){
      tcltk::tk_messageBox(type = "ok", caption = "Escape key pressed",
                           message = "Associated line list not found.  \nBase rdata file will be used.")
      return(NULL)
    }
    flat <- read.csv(flatFn, as.is = T, header = T)
    .df <-  constructDiseaseDataFrame(flat = flat, noJuris = .df)
    rm(flat, flatFn)

  } else if (NBSReportType == 0){  # unknown NBS report
    tcltk::tk_messageBox(type = "ok", caption = "Unknown report type selected",
                         message = "Base rdata file will be used.")
    return (NULL)
  }

  #----  Convert fields to proper types
  # ---- Age fields
  age_names  <- c("Age.Reported.Unit.Code","Patient.Age.At.Onset.Unit.Code")
  # ---- Integer fields
  int_fields <- c("MMWR.Year", "MMWR.Week", "Hospital.Duration", "Age.Reported")

  case.status.filter <- list(
    c("C","P"),
    c("C", "P", "N", "S", "U")
  )
  
  # ---- Main processing code
  dis.prc <- .df %>%
    # ---- Use only Tarrant County cases, to prevent non-jurisdiction cases from being counted
    dplyr::filter(County=="Tarrant County",
                  # ---- Use only cases with a valid case status
                  !is.na(Case.Status), Case.Status != "", Case.Status != "Case.Status",
                  Case.Status %in% unlist(case.status.filter[.include.all+1]),  # select desired case_status 
                  # ---- Use only cases with actual person names, this removes animal rabies
                  !is.na(Person.Name))   %>%
    # ---- Re-categorize race and ethnicity into a new Race.Eth variable
    dplyr::mutate(Race.Eth = case_when(
      Ethnic.Group == "Hispanic or Latino"       ~ "Hispanic",
      Concatenated.Race.Description == "Asian"   ~ "Asian",
      Concatenated.Race.Description == "Black or African American" ~ "Non-Hispanic Black",
      Concatenated.Race.Description == "White"   ~ "Non-Hispanic White",
      Concatenated.Race.Description == "Unknown" ~ "Unknown",
      is.na(Concatenated.Race.Description)       ~ "Unknown",
      TRUE ~ "Other/Multiracial"  ),
      # ---- Correct and standardise city spellings
      across(contains("City"), correct_city)
       ) %>%
    # ---- recategorize several variables
    dplyr::mutate(Deceased.Indicator.Code = ifelse(Deceased.Indicator.Code=="","Unk", Deceased.Indicator.Code),
                  Patient.Age.At.Onset = as.integer(Patient.Age.At.Onset)) %>%
    dplyr::mutate(Case.Status = factor(Case.Status,levels=c("C","P","S","N","U"),
                                       labels=c("Confirmed","Probable", "Suspect","Not a case","Unknown")),
                  across( c(Current.Sex.Code, Birth.Gender.Code), 
                          \(v) ifelse(v == "","U",v) |> 
                            factor(levels = c("F","M","U"),labels = c("Female", "Male","Unknown")))
                  #Current.Sex.Code = ifelse(Current.Sex.Code == "","U", Current.Sex.Code),
                  #Current.Sex.Code = factor(Current.Sex.Code, levels = c("F","M","U"),
                   #                         labels = c("Female", "Male","Unknown"))
                  )%>%
    dplyr::mutate(across(age_names, 
                         ~ factor(.,levels=c("N","H","D","M","W","Y","UNK"),  # label age code factors
                                  labels=c("Minutes","Hours","Days", "Months","Weeks","Years", "Unknown"))),
                  across(int_fields, as.integer),
                  across(matches(match = "(Date|Time)"), ~lubridate::mdy_hms(.x, truncated = 3) )
                  )|> 
    dplyr::mutate( across( where(is_yn), yn_2_logical)) |> 
    dplyr::select(-Investigation.ID) %>%
    dplyr::rename(Investigation.ID = Investigation.ID2)

  # ---- convert date fields depending on how dates are stored in the csv file
  dis <- dis.prc %>%
    # ---- Calculate Age in years for  variable AgeYrs
    dplyr::mutate(AgeYrs= case_when(
      Age.Reported.Unit.Code %in% c("Months", "Days", "Weeks", "Hours") ~ 0,  # assume less  than 1 year
      is.na(Birth.Time) ~ as.numeric(Age.Reported),   # No birth date assume years reported
      is.na(OnSet.Date) ~ as.numeric(floor(lubridate::interval(Birth.Time, Event.Date)/lubridate::duration(num=1,unit="years"))),
      Age.Reported.Unit.Code == "Years" ~
        floor(lubridate::interval(Birth.Time, OnSet.Date)/lubridate::duration(num=1,unit="years")))) %>%
    dplyr::mutate(
      # ----- calculate MMWR dates
      Event.Month = lubridate::floor_date(x = Event.Date, unit = "month") + 14,
      wk = paste(lubridate::year(Event.Date),mmwrWeek(Event.Date), sep="-")
    ) %>%
    dplyr::mutate(Onset.MMWR.Week = mmwrWeekEnd(weekNum = as.numeric(substr(wk, 6,7)),
                                                yr = as.numeric(substr(wk,1,4)))) %>%
    # ---- change types of several variables
    dplyr::mutate(across(c(Latitude, Longitude), as.numeric), 
                  across(contains("Zip"), as.character),
                  Investigation.ID = as.character(Investigation.ID),
                  AgeYrs = as.integer(AgeYrs),
                  # ---- Add Investigation.ID.trisano as an empty field
                  Investigation.ID.trisano = as.character(NA)
                  ) |> 
    # dplyr::mutate_at(.vars=vars(Latitude, Longitude), .funs = as.numeric) %>%
    # dplyr::mutate_at(.vars = vars(Investigation.ID), .funs = as.character) %>%
    # dplyr::mutate_at(.vars = vars(AgeYrs), .funs  = as.integer) %>%
    # # ---- Add Investigation.ID.trisano as an empty field
    # dplyr::mutate(Investigation.ID.trisano = as.character(NA)) %>%
    select(-wk)
  field_names_nbs_epitrax <- get_nbs_2_epitrax_fields()
  sel_flds <- names(dis)[names(dis) %in% names(field_names_nbs_epitrax)]
  # sel_flds <- c(sel_flds, "Patient.Age.At.Onset.Unit.Code", 
  #               "Age.Reported.Unit.Code", "AgeYrs", 
  #               "Event.Month", "Onset.MMWR.Week",
  #               "Race.Eth")

  dis <- dis[, sort(sel_flds)] %>%
    mutate(across(where(is.factor), as.character)) |> 
    #mutate_if(is.factor, .funs = as.character) %>%
    as.data.frame()

  rm(age_names,NBSReportType)  # clean up work space
  rm(dis.prc,.df)

  return(dis)
}


# retrieve_base_data
#
# Internally used function. Loads a parquet file that contains base data, for subsequent data analysis.
# The function loads the epi "nbs_base.parquet"  ( formerly "base.rdata") file. 
#
# @param .rdata "base.rdata" is the default.  If another folder/file name is
#  passed it will check for the existence of that file and load it.
# @return the "dis" data frame in the data file, otherwise NULL
retrieve_base_data <- function(base = file.path(paths$communicable, "NBS/nbs_base.parquet")){
  stopifnot(file.exists(base))
  dis <- nanoparquet::read_parquet(file = base)
  return(dis)
}


# constructDiseaseDataFrame
#
# internal function that takes flat and  "no jurisdiction security line list" dataframes
# then constructs a dataframe that simulates the line list with jurisdiction security.
#
# depends on the package internal reportNames variable.
#
# @param flat is the dataframe containing the report made from the Texas Flat
#   report in NBS
# @param noJuris is the dataframe containing the line list without jurisdiction
#   security report from NBS
# @return a datafram that has the same fields as the line list with jurisdiction
#   security report form NBS.  Some fields are not available so these are added
#   as fields filled with NA
constructDiseaseDataFrame <- function(flat, noJuris){
  #flat = flat
  #noJuris = dis.tmp
  #noJuris = dis.tmp
  #stop("DO NOT USE ANY NBS REPORT BESIDES THE LINE LIST  WITH JURISICTION SECURITY")

    # flat.mod is the flat dataframe modified
  flat.mod <- flat %>%
    mutate(Person.Name =  paste0(Patient.Last.Name,", ",Patient.First.Name)) %>%
    select(
      Investigation.Local.ID,
      Jurisdiction.Name,
      Person.Name,
      Birth.Time = Patient.DOB,
      Die.from.Illness               =  Die.from.this.Illness,
      Earliest.Date.Control.Initiated=  Earliest.Date.Control.Initiated,
      Earliest.Date.Suspected        =  Earliest.Date.Suspected,
      Food.Handler                   =  Food.Handler.Ind,
      Hospital.Admission.Date        =  Hospital.Admission.Date,
      Hospital.Discharge.Date        =  Hospital.Discharge.Date,
      Hospital.Name                  =  Hospital,
      Hospitalized                   =  Hospitalized.Ind,
      Last.Change.Date               =  Last.Change.Time,
      Notification.Local.ID          =  Notification.Local.ID,
      Person.Local.Id                =  Patient.Local.ID,
      Street.Address.1 =  Patient.Street.Address.1,
      Street.Address.2 =  Patient.Street.Address.2
    ) %>%
    filter(Jurisdiction.Name == "Tarrant CO Public Health Dept")

  # noSec is the noJusris dataframe with only Tarrant County records
  noSec.flt <- noJuris %>%
    filter(County == "Tarrant County",
           !grepl("-", Condition.Code )) %>%
    mutate(`Condition Code` = as.integer(Condition.Code))

  dis.cmb <- dplyr::left_join(noSec.flt, flat.mod, by = c("Investigation.ID.1" = "Investigation.Local.ID")) %>%
    # Create calculated fields
    dplyr::mutate_at(.vars = vars(contains("Date")), .funs = funs(lubridate::mdy(stringr::str_trim(substr(. ,1,10))))) %>%
    dplyr::mutate_at(.vars = vars(contains("Time")), .funs = funs(lubridate::mdy(stringr::str_trim(substr(. ,1,10))))) %>%
    dplyr::mutate(
      Hospital.Duration = Hospital.Discharge.Date - Hospital.Admission.Date,
      County.Code = ifelse(County == "Tarrant County", 48439, NA),
      State = ifelse(County == "Tarrant County", "Texas",""),
      Latitude  = NA,   # Match the NBS line list report, NBS does not geocode
      Longitude = NA
    ) %>%
    dplyr::rename(Investigation.ID2 = Investigation.ID.1)

  dis.cmb <- dis.cmb %>%
    dplyr::mutate(Date.First.Reported = pmin(dis.cmb$Report.To.County.Time, dis.cmb$Report.To.State.Date, na.rm=T))
  rm(flat.mod, noSec.flt)

  #identify missing fields and add them
  msg.flds <- setdiff(tarr:::reportNames$Jur, names(dis.cmb))
  msg.tbl <- data.frame(matrix(ncol = length(msg.flds), nrow = nrow(dis.cmb)))
  names(msg.tbl) <- msg.flds
  #msg.flds
  dis.new <- cbind(dis.cmb, msg.tbl)  # add empty columns
  dis.new <- dis.new[,tarr:::reportNames$Jur]  # use only the columns found in the line list original report

  #fn <- pathFile("data",paste0("Parital_Constructed_Line_List_",lubridate::today(),".csv"))
  #write.csv(x = dis.new, file = fn)
  #rm(fn)
  rm(dis.cmb, msg.flds, msg.tbl)
  return(dis.new)
}

# Function triSano_import
# AUGUST 22 MUST MAP FIELD NAMES AND TYPES FOR NEW TRISANO REPORT TO NBS CSV REPORT
# Helper function imports csv data from the TriSano disease list report
# Steps:
# 1. Ask the user to select the csv file
# 2. Read in the file and separate into fields if cached file does not already exist
# 3. Validate it is the file expected
# 4. Filter the records for probable and confirmed cases
# 5. Rename fields to match those used in NBS
# 6. Calculate needed fields
#    - including an Investigation.ID field that is extracted from the event.link column
# 7. Map disease conditions to NBS conditions
# Returns a dataframe with the TriSano data read and formatted for combining with NBS records
#
# Created JuLy 23, 2019
# Updated July 31, 2019
# Updated Sept 19. 2019
# Updated Nov  19, 2019
# Updated Dec  20, 2019 - implemented filter for TriSano reports
# R. Jones

#' trisano_import
#'
#' Import a TriSano line list report from a csv file. Note that the csv or text file format export from TriSano/AVR has
#' a bug. When exporting the report from TriSano, use the Excel file option then export from Excel to a csv file format.
#' This function defaults to prompt for the TriSano file to import.  Once read, field names are changed to match the NBS
#' line list  report names, probable and confirmed cases are returned by default.  If all records are desired set
#' .include.all to TRUE.
#' 
#' @param .file is the name of the csv file to open, NULL (default) the function will prompt for the file to import.
#'   When a file name is supplied no prompting occurs.
#' @param .include.all determines if all records are returned. When FALSE (default) only confirmed and probable cases
#'   are returned.  When TRUE, the caching scheme is bypassed and the user will be prompted to select the csv file to be
#'   read so that all records in the file are returned.
#'
#' @return  a data frame
#' @keywords internal
#' @export
#'
#' @examples  
#' \dontrun{
#' trisano.cases <- triSano_import()
#' }
#' @family Reading report files
#' @seealso \code{\link{nbsCSVRead}} for reading the NBS and Trisano line lists,
#'   \code{\link{nbs_import}} for importing a NBS line list report, and
#'   \code{\link{triSano_import}} for importing a TriSano line list report
#'   \code{\link{epiTrax_import}} for importing a TriSano line list report
#'
triSano_import <- function(.file = NULL, .include.all = FALSE){
  # .file = NULL
  # .include.all = TRUE

  if(is.null(.file)){
  # 1.  Ask user for data file name
    fn <- internalChooseCSV(caption = "Choose CSV file with TriSano data to open",
                            filters = tarr.filters[c("TriSano","csv"),])
  } else {
    stopifnot(file.exists(.file))
    fn <- .file
  }

  # ---- Check if this CSV file has already been processed. Load previous cached file if include.all is FALSE
  fname <- gsub(pattern=".csv",replacement = ".rdata",fn,ignore.case = T)
  if(file.exists(fname) & .include.all == FALSE){
    interactive_msg(paste0("The report file: ",basename(fname),
                " was previously processed. Loading the cached data file"))
    load(fname)
    return(triSano)
  }

  dta <- readr::read_csv(file = fn, col_names = TRUE, col_types = readr::cols())

  # 3. Further validation to occur here
  #    Check that the file has the correct field names, will need updating if
  #    triSano report changes
  tmp <- classifyCSV(dta, return.name = TRUE)
  if(tmp != "TriSanoLineList"){
    stop(paste(fn, "is not recognized as a valid TriSano line list report."))
  }
  rm(fn, tmp)

  # nms <- c("first_reported", "disease_name", "last_name",
  #          "first_name", "address", "birth_date", "gender", "ethnicity",
  #          "race", "event_date_type", "health_facility", "medical_record_number",
  #          "lab", "reporting_agency", "case_status", "outbreak", "outbreak_name",
  #          "workflow_state", "MMWR_year", "MMWR_week", "address", "city", "state")
  # if(! all(nms %in% names(dta))){
  #   err <- paste0("TriSano csv file has missing fields:\n",
  #                paste(nms[!nms %in% names(dta)], collapse = "\n"))
  #   stop(err)
  # }
  # rm(nms)


  # filter for condition used in dplyr calls below
  flts <- list(
    c("Confirmed", "Probable"),
    c("Confirmed", "Lost to follow-up", "Not a Case","Probable", "Suspect")
  )

  # 4. Filter the records for probable and confirmed cases if include.all is FALSE
  triSano <- dta %>%
    dplyr::filter(case_status %in% unlist(flts[.include.all + 1])) %>%
    dplyr::select( -medical_record_number) %>%
  # 5.a.  Format column names to use 'periods'.' instead of '_' and title case
    dplyr::rename_all(.funs = list(~stringr::str_replace_all(.,pattern = "_",replacement = "\\. "))) %>%
    dplyr::rename_all(.funs = list(~stringr::str_to_title(.))) %>%
    dplyr::rename_all(.funs = list(~stringr::str_replace_all(.,pattern = " ",replacement = ""))) %>%
    dplyr::mutate(Person.Name = paste(First.Name, Last.Name))   %>%
  # 6. Calculate fields
    dplyr::mutate(
      OnSet.Date  = case_when(
         Event.Date.Type == "Onset" ~ Date.Event,
         TRUE                       ~ as.Date(NA)),
      Event.Date.Type  = stringr::str_sub(Event.Date.Type,1,1),
      Date.Event                            = pmin(OnSet.Date, Diag.Date, First.Reported, na.rm = T),
      Age.Units                             = stringr::str_to_title(Age.Units),
                  Investigation.ID.trisano  = as.character(Record.Number),
                  Investigation.ID          = as.character(NA),
                  Age.Reported.Unit.Code    = Age.Units,
                  Age.Reported.period       = lubridate::as.period(lubridate::interval(end = First.Reported, start = Birth.Date)),
                  Age.Reported              = case_when(
                     Age.Reported.Unit.Code == "Years"   ~ as.integer(Age.Reported.period$year),
                     Age.Reported.Unit.Code == "Months"  ~ as.integer(Age.Reported.period$month),
                     Age.Reported.Unit.Code == "Days"    ~ as.integer(Age.Reported.period$day),
                     Age.Reported.Unit.Code == "Weeks"   ~ as.integer((First.Reported - Birth.Date)/7)
                  )
                ) %>%
    # ---- categorize ethinicity and race to local venacular categories
    dplyr::mutate(Race.Eth = case_when(
      Ethnicity  == "Hispanic or Latino" ~ "Hispanic",
      Race         == "Asian"   ~ "Asian",
      Race         == "Black or African American" ~ "Non-Hispanic Black",
      Race         == "White"   ~ "Non-Hispanic White",
      Race         == "Unknown" ~ "Unknown",
      is.na(Race)   ~ "Unknown",
      TRUE          ~ "Other/Multiracial"  ))  %>%
    dplyr::mutate(County      = case_when(
                     is.na(County) ~ as.character(NA),
                     TRUE          ~ paste(County,"County")),
                  Person.Name = paste(Last.Name, First.Name, sep = ", "),
                  Day.Care.associated = if_else(!is.na(Day.Care),"Yes","No"),
                  Die.from.Illness    = if_else(!is.na(Death.Date),"Yes","No")
                  ) %>%
    dplyr::select(-Record.Number, -Race, -Last.Name, -First.Name, -Lab, -Workflow.State, -Day.Care) %>%
    # ----- calculate MMWR dates
    dplyr::mutate(
      Event.Month = lubridate::floor_date(x = Date.Event, unit = "month") + 14,
      wk = paste(lubridate::year(Date.Event),mmwrWeek(Date.Event), sep="-")
    )  %>%
    dplyr::mutate(Onset.MMWR.Week = suppressWarnings(mmwrWeekEnd(weekNum = as.numeric(substr(wk, 6,7)),
                                                yr = as.numeric(substr(wk,1,4))))) %>%
    # ---- calculate ages by year and age groups
    dplyr::mutate(AgeYrs    = lubridate::as.period(lubridate::interval(start = Birth.Date, end = Date.Event))$year) %>%
    # dplyr::mutate(Age.Grp5  = age.cat(.ages = AgeYrs,.by= age.groups$Yr.5),
    #               Age.Grp10 = age.cat(AgeYrs,.by=age.groups$Yr.10),
    #               Age.Grp   = age.cat(.ages = AgeYrs,.by = age.groups$IMM)) %>%
    dplyr::select(-wk, -Age.Reported.period) %>%
    # ---- change field types to match those in the NBS report
    #      to integer
    dplyr::mutate_at(.vars = vars(Age.Onset, Hosp.Dur, Mmwr.Year, Mmwr.Week), .funs = as.integer) %>%
    #      to character
    dplyr::mutate(across( contains(c("Ill.Duration", "Zip", "Name", "phone", "ID")), as.character)) |> 
    # dplyr::mutate_at(.vars = vars(Death.Date, Ill.Dur, Zip, Outbreak.Name), .funs = as.character) %>%
    # 5.b. Rename fields to match NBS fields
    dplyr::rename(!!!field_names) %>%
    dplyr::select(-starts_with("Ra."))

      # 7. Map disease conditions to NBS conditions
  triSano$Condition <- triSano_condition(.df=triSano, Condition)
  rm(dta)

  # 8. Saved processed csv file as rdata file as part of caching scheme
  save(triSano, file = fname)
  return(triSano)
}


#' Change TriSano Condition text to match NBS
#'
#' The text in the Condition field from TriSano can be slightly different than
#' that found in NBS. The difference causes separate levels to be reported when
#' when grouping on Condition.  This functon changes the TriSano condition to
#' match that found in NBS.
#'
#' @param .df is the data frame that has the condtion text column
#' @param .col is the unquoted column name containing the condition text
#' @return a vector with the changed text equal in length as .col in .df
#' @examples
#' \dontrun{
#' tmp <- triSano_condition(.df = cases, .col = Condition)
#' cases$Condition <- tmp
#' rm(tmp)
#' }
#' @keywords internal
#' @export
triSano_condition <- function(.df, .col){
  .col <- rlang::enquo(.col)

  # named vector to recode TriSano condition values to NBS values
  # disease_map <- c(
  #   "Typhus, flea-borne (endemic,murine)"        = "Typhus fever-fleaborne, murine",
  #   "Streptococcus pneumoniae, invasive disease" = "Streptococcus pneumoniae, invasive disease (IPD)",
  #   "Streptococcal disease, invasive, Group A"   = "Streptococcus, invasive Group A",
  #   "Streptococcal disease, invasive, Group B"   = "Streptococcus, invasive Group B",
  #   "Salmonellosis, non-Paratyphi, non-Typhi"    = "Salmonella, non-Paratyphi/non-Typhi"
  # )

  .new.col <- .df %>%
    pull(!!.col) %>%
    recode(!!!disease_map)

  return(.new.col)
}


#' Update NBS data
#'
#' Updates the base file (default "base.parquet") with an NBS file.  This function is meant to be used after NBS has been
#' "frozen" for the year. The file name to select \strong{must match} the following file pattern:
#' \strong{mon_year_to_mon_year.csv}. The [nbs_process()] function is used to format the data before combining into the 
#' the base parquet file.
#'
#' All records in the chosen NBS file are used to update and replace matching records. This is strictly a time range
#' replacement.  The data range to replace is taken from the chosen file. Using this function ensures the base data file
#' matches what is in NBS.
#' 
#' @param .base a path to the base parquet file.  The default is paths$data "/NBS/nbs_base.parquet" file
#' @return  returns the updated base parquet or NULL if no file is selected
#' @export
baseNBSUpdate <- function(.base = file.path(paths$communicable, "NBS", "nbs_base.parquet")){

  nbs_fn <- internalChooseCSV(caption = paste("Choose the NBS CSV file to use in updating", basename(.base)))
  if(length(nbs_fn) == 0){
    message("No file selected")
    return (NULL)
  }

  # check that the file name matches the correct file pattern
  if(!stringr::str_detect(string = nbs_fn,pattern = "[A-z]{3}_20[0-2][0-9]_to*")){
      stop(paste0("ERROR:\nThe file name must match the pattern \'mon_year_to_mon_year.csv\' \n",
                  "where \'mon\' is the month name abbreviated such as  \'Jan\' \n",
                  "and \'year\' is the four digit year such as 2019" ))
  }

  nbs <- read.csv(file = nbs_fn, header = T, as.is = T) %>%
        nbs_process()
  rm(nbs_fn)
  
  # retrieve selected .base file 
  stopifnot(file.exists(.base))
  dis <- retrieve_base_data(.base)

  # substitute in NBS records and save to .base file
  dis <- dis %>%
    dplyr::filter(!between(Event.Date,left = min(nbs$Event.Date, na.rm=T), max(nbs$Event.Date, na.rm=T))) %>%
    rbind(nbs)

  # back up the original base file 
  if(! backup(.base)){
    stop("Creating new", basename(.base), " not done")
  } 
  
  interactive_msg(paste0("\nWriting updated '", basename(.base),"'\n"))
  write_parquet(x = dis, file = .base)
  return(dis)
}



#' Construct a base NBS file 
#' 
#' Read the csv files resulting from the NEDSS "Line List of Individual Cases with Program Area and Jurisdiction
#' Security " template report and constructs a new nbs_base.parquet. This is useful
#' when rebuilding the base file due to corruption or mistakes.
#' 
#' This function uses the [nbs_process()] function for each NBS sourced file.  
#'   
#' @param nbs_path is the folder containing exported NBS csv files. It defaults to to the "Data/Commmunicable
#'   Disease/NBS" folder. The exported NBS files must have the following naming convention:
#'   "Jan_2029_to_Dec_2029.csv" substituting the appropriate Months and years the file covers.
#' @return invisibly returns the constructed base NBS file
#' @export
construct_nbs_base <- function(nbs_path = file.path(paths$communicable,"NBS") ){
  
  nbsPth <- file.path(paths$communicable,"NBS")
  
  if(!file.exists(nbsPth)){
    stop("Folder with NBS files does not exist")
  }
  
  
  # ----  Import the data records
  # 1. Get the names of the csv files to read in using file name pattern for
  #    csv files.
  # 2. Get column names in the first data file
  # 3. Read the csv files into a list.
  # 4. Create one data frame from the list
  # ---
  
  # 1.  The file names to use must match the pattern mmm_20(0|1|2)(0-9)_to_*
  ptrn <- "[A-z]{3}_20[0-2][0-9]_to*"
  fls <- list.files(path = nbsPth, pattern = ptrn, full.names = TRUE) %>%
    .[grepl("\\.csv$",.)]
  
  btn <- winDialog("okcancel", 
                   paste("This will use",length(fls) ,
                         "files to construct and replace \nthe 'nbs_base.parquet' file. Do you wish to continue?"))
  
  if(btn == "CANCEL") return(NULL)
  
  # 2.
  column_names <- readr::spec_csv(file = fls[[1]])$cols %>%
    names()
  
  # 3.
  dfs <- map(fls,function(x) read_csv(file = x, col_types = cols(.default = "c"),col_names = column_names,skip = 1))
  
  # 4.
  allrecs <- bind_rows(dfs)
  rm(dfs,fls, ptrn, column_names)
  # ----  End of importing data records
  
  # sub a '.' for spaces spaces in the column names
  names(allrecs) <- gsub(" ",".", names(allrecs))
  
  # ---- Process records
  # Use the tarr package nbs_process function to format all variables
  # and ensure the dataframe format matches the currently used format.
  processed <- nbs_process(allrecs)
  
  rm(allrecs)  # clean up work space
  # End Record processing 
  
  # Save the nbs_base.parquet file, backing up the previous file if present
  basePth <- file.path(nbsPth,"nbs_base.parquet")
  if(file.exists(basePth)){
    # save the old parquet file
    backup(basePth)
  }
  
  
  write_parquet(x = processed, file = basePth)
  interactive_msg(paste("The range for Event Date in the the NBS base file is", paste(range(processed$Event.Date),collapse=" to ")))
  rm(basePth, nbsPth)
  invisible(processed)
}



