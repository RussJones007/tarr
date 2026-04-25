# =====================================================================
# misc_def.r
#
# Any changes to this file, requires the control_def.r script be run
#
# define Miscellaenous variables for tarr package, documented in Misc.r
# Including:
# age.groups
# city_names
# field_names
# tarr_filters
# =====================================================================


# ===== city_names_spell ============================================================
# A named list used by the package to correct city names where possible. 
# Basically this is a dictionary used by the
# correct_city function to check and correct common miss-spellings. 
# NOTE: NO longer used but kept for future reference
city_names_spell <- list(
         "Arlington"            = c("Arling?ton", "Arlin?gti?on",
                                    "Alrington", "Arlingon",
                                    "Arlingotn", "Arlington(o|g)?",
                                    "Arlingtton", "Arlignton"),
         "Benbrook"             = c("Bebbrook", "Benbrock", "Benbrooke?"),
         "Azle"                 = c("Alze", "Azel"),
         "Bedford"              = c("Bedofrd", "Beford", "Bedord"),
         "Burleson"             = c("Bureleson", "Burlenson",
                                    "Burlesen", "Burlseson"),
         "Colleyville"          = c("cooleyville", "COLLLEYVILLE",
                                    "Colleyvillle"),
         "Crowley"              = c("Croweley", "Croowley",
                                    "crowly"),
         "Dalworthington Gardens" = c("Dalworthington Gardens?",
                                     "Dalworthington Grdns"),
         "Edgecliff Village"    = c("Edge ?vi?ll?"),
         "Euless"               = c("Eules(s|a){1,3}"),
         "Forest Hill"          = c("forr?est? ?(h|s)ill"),
         "Fort Worth"           = c("(Fort *W(o|q|r)(r|o)(t|h)(h|t|k))",
                                    "FtW", "F. Worth",
                                    "Ft\\. *Worth?", "Forth? ?Worth",
                                    #"Fort  Worth",      # catches double space
                                    "Ft. Worth", "Ft. Wth"),
         "Grapevine"            = c("gr?aprevine"),
         "Grand Praire"         = c("grande? ?prairie"),

         "North Richland Hills" = c("NRH",
                                    "North Richland",
                                    "Nort Richland Hills",
                                    "N\\. Richla?nd Hills?",
                                    "N Richland Hills"),
         "Grand Prairie"        = c("Grand Praire", "Gran Prairie",
                                    "Grand Priaire", "Grand Priairie",
                                    "Grand Praire"),
         "Southlake"            = c("South ?Lake",
                                   "S. Lake",
                                   "Soutlake"),
         "Keller"               = c("Kelelr"),
         "Haslet"               = c("Hasley"),
         "Mansfield"            = c("Masfield",
                                    "Mansfeild"),
         "Haltom City"          = c("Halton", "Haltm"),
         "Watauga"              = c("watagua"),
         "Westworth Village"    = c("Westworth vill"),
         "White Settlement"     = c("whi?te? settle")
       )

# ===== city_names ==================================================================
# Names of cities within Tarrant County.
city.names <- c("Arlington", "Azle", "Bedford", "Benbrook", "Blue Mound", "Burleson",
            "Colleyville", "Crowley", "Dalworthington Gardens", "Edgecliff Village",
            "Euless", "Everman", "Flower Mound", "Forest Hill", "Fort Worth",
            "Grand Prairie", "Grapevine", "Haltom City", "Haslet", "Hurst",
            "Keller", "Kennedale", "Lake Worth", "Lakeside", "Mansfield",
            #"No City", 
            "North Richland Hills", "Pantego", "Pelican Bay", "Reno",
            "Richland Hills", "River Oaks", "Roanoke", "Saginaw", "Sansom Park",
            "Southlake", "Trophy Club", "Watauga", "Westlake", "Westover Hills",
            "Westworth Village", "White Settlement", "Joint Reserve Base", "Naval Air Station")

# Names of cities outside of Tarrant County many are in adjacent counties.  Useful for the
# fuzzy matching algorithm in the correct_city function. Many of the city names
# below are provided to differentiate between Tarrant cities and cities outside of Tarrant County.
city.names.outside <- c("Aledo", "Allen", "Dallas", "Houston", "Denton","Cleburne",
                        "Irving", "Lexington", "Edinburg", "Garland", "Midlothian",
                        "Alvarado", "Joshua", "Denton", "Plano", "Llano", "Mckinney",
                        "Weatherford", "Carollton", "Mesquite", "Ducanville",
                        "Addison", "Lancaster", "DeSoto", "Highland Park", 
                        "Willow Park", "Springtown", "Hudson Oaks", "Granbury",
                        "Rhome", "Boyd", "Red Oak", "Italy", "Waxahachie", "Ennis",
                        "Angleton", "Clinton", "Farmington", "Burlington", 
                        "Toledo", "Abilene", "Wellington", "Denison","Houston",
                        "Boston", "Livingston", "Blanco", "Pleasanton", "Live Oak",
                        "Sinton", "Spring", "Rochester Hills", "Seabrook", 
                        "Northlake", "Pearland", "New Boston", "Grand Saline",
                        "Grand Isle", "Covington", "Canton", "Mt Pleasant",
                        "Lone Oak", "Bellville", "Sherman")

# =====   field names to emulate NBS fields used ===================================
# The name of the element is the NBS field.  Mapped to the name is the
# field name from the TriSano report. i.e., NBS.Name = TriSano.Name
# field_name mapping
#
# A named vector of field names used to ensure columns are named the same after
# reading in data from NBS or TriSano sources.  The names are NBS related,
# while the actual values are TriSano derived. i.e., NBS.Name = TriSano.Name
field_names <- c(
           "Report.Date"                    = "First.Reported",
           "Patient.Age.At.Onset.Unit.Code" = "Age.Units",
           "Patient.Age.At.Onset"           = "Age.Onset",
           "Person.Name"                    = "Person.Name",
           "Case.Status"                    = "Case.Status",
           "County"                         = "County",
           "Birth.Time"                     = "Birth.Date",
           "Current.Sex.Code"               = "Gender",
           "Ethnic.Group"                   = "Ethnicity",
           "Condition"                      = "Disease.Name",
           "Event.Date"                     = "Date.Event",
           "Event.Type"                     = "Event.Date.Type",
           "Hospital.Name"                  = "Health.Facility",
           "Report.Source.Name"             = "Reporting.Agency",
           "Outbreak.Indicator"             = "Outbreak",
           "MMWR.Year"                      = "Mmwr.Year",
           "MMWR.Week"                      = "Mmwr.Week",
           "Street.Address.1"               = "Address",
           "Zip.Code"                       = "Zip",
           "Latitude"                       = "Lat",
           "Longitude"                      = "Lon",
           "Patient.home.phone"             = "Phone",
           "Primary.Language"               = "Lang",
           "Physician.Name"                 = "Clinician",
           "Diagnosis.Date"                 = "Diag.Date",
           "Deceased.Time"                  = "Death.Date",
           "Report.To.State.Date"           = "Notified.Dshs.Date",
           "Hospital.Admission.Date"        = "Hosp.Admit",
           "Hospital.Discharge.Date"        = "Hosp.Discharge",
           "Hospital.Duration"              = "Hosp.Dur",
           "Illness.Duration"               = "Ill.Dur",
           "Illness.End.Date"               = "Ill.End.Date",
           "Investigation.Start.Date"       = "Inv.Start",
           "Investigator.Assigned.Date"     = "Inv.Assigned",
           "Investigator.Name"              = "Investigator",
           "Report.Source.Type"             = "Ra.Type",
           "Reporter.Name"                  = "Reporter",
           "Reporter.Phone"                 = "Ra.Phone",
           "City"                           = "City",
           "State"                          = "State",
           "Pregnant"                       = "Pregnant",
           "Food.Handler"                   = "Food.Handler",
           "Hospitalized"                   = "Hospitalized",
           "Outbreak.Name"                  = "Outbreak.Name",
           "OnSet.Date"                     = "OnSet.Date",
           "Investigation.ID.trisano"       = "Investigation.ID.trisano",
           "Investigation.ID"               = "Investigation.ID",
           "Age.Reported.Unit.Code"         =  "Age.Reported.Unit.Code",
           "Age.Reported"                   = "Age.Reported",
           "Race.Eth"                       = "Race.Eth",
           "Day.Care.associated"            = "Day.Care.associated",
           "Die.from.Illness"               = "Die.from.Illness",
           "Event.Month"                    = "Event.Month",
           "Onset.MMWR.Week"                = "Onset.MMWR.Week",
           "AgeYrs"                         = "AgeYrs"  )


# ===== disease_map =========================================================================================
# is a named vector that translates between EpiTrax and NBS conditions.
# The name is the EpiTrax condition, the value is the NBS condition
# This is useful for converting EPiTrax condition names to NBS names and vice versa.
cross_walk <- read_xlsx(path = "data-raw/NBS and EPiTax Conditions.xlsx", sheet = 1, na = "NA") |> 
  filter(! is.na(EpiTrax_Condition))

disease_map <- cross_walk$NBS_Condition |> set_names(cross_walk$EpiTrax_Condition)
rm(cross_walk)

# ======= tarr.filters ===============================================================
# file selection filters used in choosing files
tarr.filters <- matrix(c(
                "NBS csv report file (NBS_*.csv)",        "NBS_*.csv",
                "TriSano csv report file (TriSano*.csv)", "TriSano_*.csv",
                "EpiTrax csv report file (epitrax, ET)",  "EpiTrax_*.csv",
                "Comma Separated file (*.csv)",           "*.csv",
                "Text files (*.txt)",                     "*.txt",
                "Excel files (*.xls*)",                   "*.xls*"),
               dimnames = list(c("NBS","TriSano", "EpiTrax", "csv", "txt", "Excel")),
               ncol = 2, byrow = TRUE)



# A named character vector for simplifying and collapsing condition names ------------------------------------------
# This named vector is used in simplify_term function to the simple_term argument.  The named element (left side) 
# is the regex used in a pattern, and the value element (right side) is the replacement argument.
# Characters that  would normally be regular expression operators are escaped with a "\\"
simple_conditions <- 
  c("Haemophilus influenzae, invasive disease"                 = "Haemophilus influenzae, invasive",
    "Hepatitis A, acute"                                       = "Hepatitis A", 
    "Carbapenem-resistant Enterobacteriaceae \\(CRE\\)"        = "Carbapenem-resistant Enterobacteriaceae",
    "Candida auris, colonization/screening"                    = "Candida auris, colonization",
    "Hemolytic uremic"                                         = "Hemolytic Uremic Syndrome (HUS)",
    "Monkeypox"                                                = "Mpox",
    "Neisseria meningitidis, invasive \\(Mening\\. disease\\)" = "Neisseria meningitidis",
    "Shiga toxin-producing Escherichia coli \\(STEC\\)"        = "STEC/Escherichia coli",
    "Streptococcus pneumoniae, invasive disease \\(IPD\\)"     = "Strep. pneumoniae, invasive",
    "Streptococcus, invasive Group A"                          = "Strep. Group A, invasive",
    "Streptococcus, invasive Group B"                          = "Strep. Group B, invasive",
    "Zika virus disease, non-congenital"                       = "Zika disease, non-congenital",
    "Zika virus infection, non-congenital"                     = "Zika infection, non-congenital",
    "Zika virus disease, congenital"                           = "Zika disease, congenital",
    "Zika virus infection, congenital"                         = "Zika infection, congenital",
    "Chikungunya virus disease"                                = "Chikungunya", 
    "Salmonella, non-Paratyphi/non-Typhi"                      = "Salmonellosis",
    "Haemophilus influenzae, invasive"                         = "Haemophilus influenzae, invasive*",
    "Chickenpox"                                               = "Varicella (Chickenpox)", 
    "West Nile Fever"                                          = "West Nile non-neuroinvasive",
    "West Nile Virus, Non-neuroinvasive"                       = "West Nile non-neuroinvasive",
    "West Nile Virus Non-neuroinvasive disease"                = "West Nile non-neuroinvasive",
    "West Nile Virus, Neuroinvasive"                           = "West Nile neuroinvasive",
    "West Nile Encephalitis"                                   = "West Nile neuroinvasive", 
    "West Nile Virus neuroinvasive disease"                    = "West Nile neuroinvasive",
    "Encephalitis, West Nile"                                  = "West Nile neuroinvasive", 
    "Vibrio vulnificus infection"                              = "Vibriosis, non-cholera species",
    "Vibrio parahaemolyticus"                                  = "Vibriosis, non-cholera species",
    "Vibriosis, other or unspecified"                          = "Vibriosis, non-cholera species",
    "non-cholera Vibrio species"                               = "Vibriosis, non-cholera species"
  )
