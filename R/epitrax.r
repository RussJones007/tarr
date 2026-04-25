# epitrax.r
# Functions to handle EpiTrax data 
# Exported functions for the user are epitrax_import() for reading and proceesing epitrax line lists
# and epitrax_2_nbs() to convert field names and types to match an the nbs line list

#' Import EpiTrax line list
#'
#' Imports and processes a line list report from EpiTrax, **without de-duplication**. If a file name is not given, it
#' will open a window for the user to select a csv file.  A caching scheme is used such that if the file has been read
#' before it will be returned. Adds the following fields: event_month, onset_mmwr_week, and age_yrs. The returned 
#' data frame may be passed to the [epitrax_2_nbs_format()] function resulting in a data frame that may be combined with a 
#' data frame derived from NBS.
#' 
#' At a minimum the EpiTrax file should have the following column names: person_last_name, person_first_name,
#' patient_lhd_case_status, patient_disease, patient_event_onset_date, patient_event_type, patient_birth_date,
#' patient_birth_sex, patient_ethnicity, patient_reporting_agency, patient_mmwr_year, patient_mmwr_week, and
#' patient_phone_number.
#'
#' @param .file is the name of the csv file exported from EpiTrax, if NULL (default) the function will prompt for the
#'   file to import.
#' @param .include.all determines if all records are returned. When FALSE (default) only confirmed and probable cases
#'   are returned.  When TRUE, the caching scheme is bypassed and the user will be prompted to select the csv file to be
#'   read. When the CSV is read all records that are Confirmed, Probable, Suspect, and Not A Case are returned.
#' @param .use_cache When TRUE (default) a previously processed file will be returned. Setting to  FALSE causes the 
#' EpiTrax csv file to  be processed regardless if it has been processed before.  If .include.all is set to TRUE, this 
#' argument has no effect.  CURRENTLY THIS ARGUMENT IS NOT USED.
#' @param .return_missing_county will find and return the records where the county field is missing (NA)
#'
#' @return A data frame with data processed and fields added.  If no file is selected returns an empty data frame.
#' @export
#'
#' @examples
#' \dontrun{
#' epitrax.cases <- epi_trax_import(.file = "EpiTrax_cases_April 2023")
#' }
#' @family Reading report files
#' @seealso [nbs_import()] for importing a NBS line list report.
#' 
epitrax_import <- function(.file = NULL, .include.all = FALSE, .use_cache = TRUE, .return_missing_county = FALSE){

  if(is.null(.file)){
    # 1.  Ask user for data file name
    fn <- internalChooseCSV(caption = "Choose the year to date CSV file with EpiTrax data to open",
                            filters = tarr.filters[c("EpiTrax","csv"),])
    # No file selected
    if(length(fn) == 0) {
      interactive_msg("No EpiTrax report file selected\n")
      return(data.frame())
    }
  } else {
    stopifnot(file.exists(.file))
    fn <- .file
  }
  
  # ---- Check if this CSV file has already been processed. Load previous cached file if include.all is FALSE
  fname <- gsub(pattern=".csv", replacement = ".parquet", x = fn, ignore.case = T)
  if(file.exists(fname) & .include.all == FALSE & .use_cache == TRUE){
    interactive_msg(paste0("The report file: ",basename(fname),
                 " was previously processed. Returning the cached data file"))
    ret <- nanoparquet::read_parquet(fname)
    return(ret)
  }
   
  dta <- readr::read_csv(file = fn, col_names = TRUE, col_types = readr::cols(.default = readr::col_character())) |> 
    janitor::clean_names()
  
  # 3. Further validation to occur here
  #    Check that the file has the correct field names, will need updating if the
  #    EpiTrax report changes
  tableType <- classifyCSV(dta, return.name = TRUE)
  if(tableType != "EpiTraxLineList"){
    stop(paste(basename(fn), "is not recognized as a valid EpiTrax line list report."))
  }
  rm(tableType)
  
  # filter for condition used in dplyr calls below
  flts <- list(
    c("Confirmed", "Probable"),
    c("Confirmed", "Lost to follow-up", "Not a Case","Probable", "Suspect")
  )
  
  # 4. Filter the records for probable and confirmed cases if include.all is FALSE, otherwise all records
  ret <- dta %>%
    dplyr::filter(patient_lhd_case_status %in% unlist(flts[.include.all + 1])) %>%
    dplyr::mutate(address_at_diagnosis_county      = case_when(
      is.na(address_at_diagnosis_county) ~ as.character(NA),
      TRUE          ~ paste(address_at_diagnosis_county,"County")),
    ) %>%
    # ----- calculate MMWR dates
    dplyr::mutate(
      # convert character dates to actual dates
      across( ends_with("_date"), \(x) ymd_hms(x, truncated = 3)),  
      event_month = lubridate::floor_date(x = patient_event_onset_date, unit = "month") + 14,
      wk = paste(lubridate::year(patient_event_onset_date),mmwrWeek(patient_event_onset_date), sep="-")
    )  %>%
    dplyr::mutate(onset_mmwr_week = suppressWarnings(mmwrWeekEnd(weekNum = as.numeric(substr(wk, 6,7)),
                                                                 yr = as.numeric(substr(wk,1,4))))) %>%
    # ---- calculate ages by year and age groups
    dplyr::mutate(age_yrs    = rage::calculate_age(
      start = as.Date(patient_birth_date), 
      end   = as.Date(onset_mmwr_week)) |> as.integer()) %>%
    dplyr::select(-wk) %>%
    # ---- change field types to match those in the NBS report
    #      to integer
    dplyr::mutate(across(c(patient_mmwr_year, patient_mmwr_week), as.integer)) |> 
    dplyr::mutate(Zip.Code = as.character(address_at_diagnosis_zip),
                  across( c(address_at_diagnosis_lat, address_at_diagnosis_lon), as.numeric))
    
  # check that address_at_diagnosis_county is complete, if not show a dialog to inform the user
  msg_ndx <- check_col_NA(dta, address_at_diagnosis_county)
    if( length(msg_ndx) > 0){
      if(! .return_missing_county & interactive()){
        rstudioapi::showDialog("Required Data Missing",
                               message = paste("The data file", basename(fn), "is missing",
                                               length(msg_ndx), "entries for the address_at_diagnosis_county coulmn. You can get those records ",
                                               "with the missing data by calling the epitrax_import() function with the argument",
                                               ".return_missing_county = TRUE"))
      } else {
        return(ret[msg_ndx, ])
      }
    }
  
    # 8. Saved processed csv file as parquet file as part of caching scheme
  # Removed due to persistent errors in write_parquet
  #if( .include.all == FALSE)  write_parquet(x = ret, file =  fname)
  return(ret)
}


# EpiTrax to NBS format ------------------
# checks if all of the quoted column names in '....' of et  exist.  If so returns TRUE.  Used internall in the following
# internal functions.
columns_missing <- function(et, ...){
  all(! map_lgl(list(...), \(col) is.null(et[["col"]])) )
}

# functions that are used to translate data frame columns

#' Age calculation 
#' 
#' With a birth and end dates calculates the age.  This is based on the [calc_age_unit()] function, but internally
#' memoised.  Using the [calc_age_unit()] function with the same arguments results in a rapid return. 
#'
#' @param et epitrax line list
#' @param birth,end the quoted name of the field with the birth date and end is the event date, onset, collection date, etc
#' @return in the case of age it will be an integer, for unit it will be "years, months, or days".
#' @keywords internal
calc_age_field <- function(et, birth, end){
  if(columns_missing(et = et, birth, end)) return(NULL)
  age_auto_unit(et[[birth]], et[[end]]) |> 
    as.integer()
}

#' Age unit calculation 
#'
#' @param et a data frame with dates to calculate age or date intervals
#' @param birth,end the quoted name of the field with the birth and end dates like event date, onset, collection date, etc.
#' @return The age unit (i.e. Years, Months or Days).
#' @keywords internal
calc_age_unit <- function(et, birth, end){
  if(columns_missing(et = et, birth, end)) return(NULL)
  age_auto_unit(et[[birth]], et[[end]]) |> 
    names()
}

#' Takes two names and creates one entry
#'
#' @param et an epitrax line list
#' @param first name 
#' @param last name
#' @return "last, first"
#' @keywords internal
calc_name <- function(et, first, last){
  if(columns_missing(et, first, last)) return(NULL)
  bothNA <- (is.na(et[[last]]) & is.na(et[[first]]))
  ret <- character(length(et[[last]]))
  ret <- paste(et[[last]], et[[first]], sep = ", ")
  ret[bothNA] <- NA_character_
  return(ret)
}

#' Calculate the days between two events
#'
#' @param et an epitrax line list
#' @param start,end the start and end date vectors
#'
#' @return An in
#' @keywords internal
calc_duration <- function(et, start, end){
  if(columns_missing(et, start, end)) return(NULL)
  
  na_lgl <- `!`(is.na(et[[end]]))
  ret <- integer(length(et[[start]]))
  ret[na_lgl] <- difftime(et[[end]][na_lgl], et[[start]][na_lgl], units = "days") |> as.integer() 
  ret
}

#' Convert an EpiTrax line list to NBS format
#' 
#' Takes an EpiTrax line list data frame and converts the field names and types to those in the NBS report line list with
#' jurisdiction security. The NBS line list report may be supplied as a template in which case only the fields in that
#' report are returned.  NOTE that this function does not do  **any de-duplication**!
#'
#' @param et a data frame read in from an EpiTrax csv file. See the [epitrax_import()] function.
#' @param nbs NBS formatted data frame.  Defaults to the NBS line list with juridcition security
#'
#' @return a data frame with column names and data types that match the NBS data frame. The values in the "Condition" 
#' column match those from NBS.
#' @export
epitrax_2_nbs_format <- function(et, nbs = reportNames[["Jur"]]){
  
  # This function uses the nbs argument as a template of fields to process and return in the NBS format.
  if(is.null(nbs)) stop("Do not use 'NULL' for the 'nbs' argument")

  field_names_nbs_epitrax <- get_nbs_2_epitrax_fields()
  
  # Select those in field_names_nbs_epitrax where the names appear in the nbs data frame.
  flds <- names(field_names_nbs_epitrax)[names(field_names_nbs_epitrax) %in% names(nbs)]
  nbs <- nbs[flds]
  sel_list <- field_names_nbs_epitrax[flds]  # these are the list items that exists in the nbs data frame
  
  # Convert character columns that have date information to POSIXct
  # identify columns that are character and have "_date" in the name
  char_date_lgl <- imap_lgl(et, \(col, nm) str_detect(nm, "_date") & is.character(col))
  char_date_columns <- names(et)[char_date_lgl]
  et <- et |> 
    mutate(across(all_of(char_date_columns), \(d) parse_date_time2(d, orders = c("Ymd", "Ymd HMOS2")))
    )
  
  # identify sel_list entries that are simple renames
  fld_renames <- keep(sel_list, .p = is.character) # Get the columns that are just renames
  
  # start the returned data frame by converting yes/No columns
  # and renaming the simple columns to the new name
  ret_et <- et[as.character(fld_renames)] |> # select the columns needed
    # convert the Yes/No/Unk variables to logical
    mutate( across( where(is_yn), yn_2_logical)
    ) |> 
    # rename the columns that are not calculated fields
    rename(!!! fld_renames)                              

  rm(char_date_lgl, char_date_columns, fld_renames)

  # now run the functions for the needed calculated fields
  fld_calc <- keep(sel_list, is.function)
  iwalk(fld_calc, \(f,name) ret_et[[name]] <<- f(et))
  
  # select the fields to return
  ret_et <- ret_et[names(sel_list)]
  
  # ID the discordant column types between data frames and attempt to convert them to the same type
  flds <- janitor::compare_df_cols(nbs, ret_et) 
  discordant <- flds[flds[[2]] != flds[[3]], ]
  walk(discordant[["column_name"]], \(nm) ret_et[[nm]] <<- convert_type(nbs[[nm]], ret_et[[nm]]))
  
  # Map disease conditions to NBS conditions
  ret_et$Condition <- triSano_condition(.df = ret_et, Condition)

  return(ret_et)
}


#' Get EpiTrax to NBS fields
#'
#'  Internal only function that returns the vector used to translate EpiTrax fields to NBS fields. 
#'
#' @return A named vector.  The names are the NBS field names and the values are the EpiTrax field names or functions
#' that are used to translate the EpiTrax field(s) to NBS
#' @noRd
get_nbs_2_epitrax_fields <- function(){

# Vector for translating from EpiTrax fields to NBS fields --------------------------------------------------------
# a named character vector of field names.  On the right are the NBS names after reading using read.csv. The values
# on the right are the EpiTrax field names. 
field_names_nbs_epitrax <- c(
  "Report.Date"                  =  "patient_first_reported_ph_date",
  "Patient.Age.At.Onset"         =  \(et_df) calc_age_field(et_df, "patient_birth_date", "patient_disease_onset_date"),
  "Patient.Age.At.Onset.Unit.Code" = \(et_df) calc_age_unit(et_df, "patient_birth_date","patient_disease_onset_date"),
  "Age.Reported"                 =  \(et_df) calc_age_field(et_df, "patient_birth_date", "patient_first_reported_ph_date"),
  "Age.Reported.Unit.Code"       =  \(et_df) calc_age_unit(et_df, "patient_birth_date", "patient_first_reported_ph_date"),
  # Match the NBS processed file calculated field for ages in years
  "AgeYrs"                       =  \(et_df) rage::calculate_age(start = as.Date(et_df[["patient_birth_date"]]), as.Date(et_df[["patient_event_onset_date"]])),  
  "Person.Name"                  =  \(et_df) calc_name(et_df, "person_first_name", "person_last_name"),
  "Case.Status"                  =  "patient_lhd_case_status",
  "Condition"                    =  \(et_df) disease_map[ et_df[["patient_disease"]] ],  # change the EpiTrax condition to the NBS condition
  "OnSet.Date"                   =  "patient_disease_onset_date",
  "Event.Date"                   =  "patient_event_onset_date",
  "Event.Type"                   =  "patient_event_type",
  "Die.from.Illness"             =  "condition_caused_death",
  "Deceased.Indicator.Code"      =  "patient_died",
  "Deceased.Time"                =  "patient_date_of_death",
  "Birth.Time"                   =  "patient_birth_date",
  "Birth.Gender.Code"            =  "patient_birth_sex",
  "Current.Sex.Code"             =  "patient_current_gender",
  "Ethnic.Group"                 =  "patient_ethnicity",
  "Concatenated.Race.Description" = \(et_df) stringr::str_replace_all(et_df[["patient_race"]],";", ", " ), 
  "Pregnant"                     =  "patient_pregnant",
  "Day.Care.associated"          =  "patient_day_care_association",
  "Food.Handler"                 =  "patient_food_handler",
  "MMWR.Year"                    =  "patient_mmwr_year",
  "MMWR.Week"                    =  "patient_mmwr_week",
  "Street.Address.1"             =  "address_at_diagnosis_street",
  "Street.Address.2"             =  "address_at_diagnosis_unit_number",
  "Zip.Code"                     =  "address_at_diagnosis_zip",
  "County"                       =  \(et_df) paste(et_df[["address_at_diagnosis_county"]], "County"),
  "City"                         =  \(et_df) correct_city(et_df[["address_at_diagnosis_city"]]),
  "State"                        =  \(et_df) (state.name |> set_names(state.abb))[ et_df[["address_at_diagnosis_state"]] ],
  "Latitude"                     =  "address_at_diagnosis_lat",
  "Longitude"                    =  "address_at_diagnosis_lon",
  "Outbreak.Name"                =  "outbreak_name",
  "Outbreak.Indicator"           =  "patient_outbreak_associated",
  "Patient.home.phone"           =  "patient_phone_number",
  "Primary.Language"             =  "person_primary_language",
  "Report.Source.Name"           =  "patient_reporting_agency",
  "Physician.Name"               =  \(et_df) calc_name(et_df, "clinician_first_name", "clinician_last_name"),  
  "Diagnosis.Date"               =  "patient_date_diagnosed",
  "Hospitalized"                 =  "patient_hospitalized_for_condition",
  # The epitrax person_facility_name can be a hospital, urgent care, clinic, etc.  The NBS Hospital.Name is only hospitals
  "Hospital.Name"                =  "person_facility_name",
  "Hospital.Admission.Date"      =  "patient_visit_start_date",
  "Hospital.Discharge.Date"      =  "patient_visit_end_date",
  "Hospital.Duration"            = \(et_df) calc_duration(et_df, "patient_visit_start_date","patient_visit_end_date"),
  "Hospital.Street.Address"      =  "person_facility_street",
  "Hospital.City"                =  \(et_df) correct_city(et_df[["person_facility_address_city"]]),
  "Hospital.Zip"                 =  "person_facility_address_zip",
  #"Hospital.County"              =  "person_facility_county",
  "Investigation.Start.Date"     =  "first_investigation_started_date",
  "Investigation.ID"             =  "patient_record_number",
  "Investigator.Assigned.Date"   =  "first_assigned_to_investigator_date",
  "Investigator.Name"            =  "patient_investigator",
  "Report.Source.Type"           =  "person_facility_type",
  "Reporter.Name"                =  \(et_df) calc_name(et_df, "patient_reporter_first_name", "patient_reporter_last_name"), 
  "Reporter.Phone"               =  "patient_reporter_phone_phone_number",
  "Disease.Imported"             =  "patient_imported_from",
  "Investigation.ID.trisano"     =  "patient_event_id",          # added field to show the event ID from EpiTrax
  "Event.Month"                  =  \(et_df) lubridate::floor_date(x = et_df[["patient_event_onset_date"]], unit = "month") + 14,
  "Onset.MMWR.Week"              =  \(et_df) mmwrWeekEnd(weekNum = mmwrWeek(et_df[["patient_event_onset_date"]]), yr = lubridate::year(et_df[["patient_event_onset_date"]])),
  "Race.Eth"                     =  \(et_df) dplyr::case_when(
    et_df[["patient_ethnicity"]]    == "Hispanic or Latino" ~ "Hispanic",
    et_df[["patient_race"]]         == "Asian"   ~ "Asian",
    et_df[["patient_race"]]         == "Black or African American" ~ "Non-Hispanic Black",
    et_df[["patient_race"]]         == "White"   ~ "Non-Hispanic White",
    et_df[["patient_race"]]         == "Unknown" ~ "Unknown",
    is.na(et_df[["patient_race"]])   ~ "Unknown",
    TRUE          ~ "Other/Multiracial"  )
)

# ensure no duplicates 
field_names_nbs_epitrax <- field_names_nbs_epitrax[! duplicated(names(field_names_nbs_epitrax))]
field_names_nbs_epitrax <- field_names_nbs_epitrax[! duplicated(field_names_nbs_epitrax)]

return(field_names_nbs_epitrax)
}



