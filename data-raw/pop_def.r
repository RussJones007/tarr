# ====================================================================================== 
# pop_def.R 
# This script processes data originated from the Texas Demographic Center. Uses the population projection 2010-2050 file
# from the Texas Demographic Center and the year specific estimate csv files to construct one data frame. This script is
# part of the tarr package. It saves a copy of the 'pop' data frame for  use by the package
# 
# Population estimates from the Census Bureau for Tarrant Zip codes are also available from 2011-2019.
#
# Russ Jones
#  Revised August 11, 2023
#    - Removed the "type" field from the tables.  
#    - All population tables are part of the population list and documented.
#    - The census table was updated to calculate and include total population by sex, previously only the "All" 
#      category had population.
#    - Five population tables available - census, census.estimates, texas.estimates, texas.projections, and zcta
#   
#  Revised July 2023
#   - Migration scenario columns removed as the SDC did not include them in the 2018 vintage projections.
#   - Later years do include migration scenario.  When downloading new years, use the 1.0 scenario
#   - Added two functions for encoding and decoding ages and age groups from character to iv (intervals) and vice versa.
# 
#  Revised June 2023 
#    - Encode age groups as ivs intervals.  
#    - Interval for an age group are encoded as left closed and right open intervals.  For example, [10, 15)  means all
#      numbers from 10 up to but not including 15.
#    - Functions age_group_encode and age_group_decode were removed
#    - Added functions as.age_group and iv_to_age_group that convert age groups or ivs intervals to each other. 
#
# Revised March 2023.  Age group encoding is dropped as it added too much complexity.
#
# Revised June 4, 2022.  Estimates for 2011 through 2019 are processed by a separate script and saved in individual
# folders in "Population/Estimates/Texas Demographic Center/ (county, asre, place)" This simplifies this script to read
# the final estimate files from those folders.
#
# Revised January 8, 2022 - an encoding scheme was implemented for the age.num field to handle age groups.
#     - A single age is shown as that number e.g., "2" in the age.car field is 22 in the age.num field
#     - 9000 is reseved for all ages.
#     - Age groups are coded for numbers above 9000.  The encoding is # '9lluu'. Meaning 9, the (ll)ower limit number, 
#       and the (uu)pper digit number.  The lower and upper limits will always consist of two digits. For example, an 
#       age group of 5-15 is shown as 90515. Another example for ages 0 through 1 is 90001.
#
# Two functions are used to encode and decode age groups:
#     - age_group_encode(age.grp)
#     - age_group_decode(age.grp)
#
#
# Revised January 1, 2022 - Added decennial census figures from 2000 and 2010 The general 2020 census data was not yet
# available - just PL1 information with race-ethnicity data.
#
# Revised April 2021 - due to changes in the 2019 projections file from the state demographic center, county.name was
# changed to area.name. Race ethnicity changed from the old term "Anglo" to the "NH-White".  Asian has been split from
# "Other" Migration scenario for projections were dropped. One scenario, that for 2010-2015 census estimates was used
# for the scenario. See "Methodology 2018 Projections of the Population of Texas and Counties in Texas by Age,.pdf" in
# the projections folder. 
# 
# Revised November 2019 
#   - added 2018 population estimates from the Texas Demographic center. 
#   updated the coding for importing estimate csv files to be more robust. 
# 
# Revised June 20, 2019 
# 
#   - added ZCTA population counts and estimates for 2010 through 2017.  To prevent loading of a large data set, Tarrant 
#   zip code information is stored separately. Revised May 27, 2019 - Loaded the population estimates for 2000-2009 
#   
# Revised 5/31/2018 
#    - made the table in long format, added a "Type" field for "Estimate" versus "Projection" 
#    
# Revised 8/14/18 
#     - made this script part of the tarr package and placed in the data-raw folder.  It is  used to update the pop 
#     dataframe variable.  The Data  frame is saved in two places: one in the population folder found in paths, and the 
#     other in the R folder of the tarr package as sysdata.rda
# 
# Revised 8/15/18 
#   - When this scrpt is finished, it removes all variables created except 'pop'. The script is meant to be called by 
#   control_def.r which saves pop and other data frames to sysdata.rda
# 
# Note: The Texas State Data Center Projections are made every two years and changes do occur with updates. Estimates
# are stable and generally do not change with updates. Mid-year estimates are released for two years before the current
# year. For 2001-2009 estimates this script is dependent on the year all county, age, sex, race estimate csv files
# downloaded from the Texas Demographic Center and named as "YYYY_ASRE_Estimate_alldata.csv" in a folder of "Estimates"
# under the population folder.  The link to download estimate files is https://demographics.texas.gov/Estimates/ and
# select the "Age, Sex, and Race/Ethnicity" files. Population projections are found in a downloaded file from the SDC
# and named as "State Data Center Tarrant_County 2010-2050 Single year age.csv" Projections may be downloaded from
# http://txsdc.utsa.edu/Data/TPEPP/Projections/. Estimates for 2001-2009 were downloaded from the DSHS Center For Health
# Statistics site. These files were originally sourced by DHS from the Texas Demographic Center, but used aggregated age
# groups.
# ======================================================================================
# --- Counties to include in the saved population data
counties <-  c("Tarrant", "Dallas","Harris", "Travis", "Bexar", "El Paso", 
               "Rockwall", "Williamson", "Nueces", "Lubbock", "Johnson", "Parker", 
               "Wise", "Denton", "Ellis", "Collin","Kaufman", "Fort Bend", 
               "Hidalgo", "Texas")

# convenience function for sorting values for levels when making a factor
sort_values <- compose(sort, unique)

# ==== Projections processing =======
print("Constructing population projection tables")
fn <- paste0(paths$population, "/Projections/State Data Center Tarrant_County 2010-2050 Single year age.csv")
#fn <- file.path(paths$population, "projections", "2018_vintage_2010-2025_projections.csv")
file.exists(fn)

col.types <- cols(
    # migration_scenario_num  = col_double(),
    # migration_scenario_char = col_character(),
    year            = col_integer(),
    FIPS            = col_character(),
    area_name       = col_character(),
    age_in_yrs_num  = col_double(),
    age_in_yrs_char = col_character(),
    .default        = col_double()
    )

process_age_char <- compose(
  ~ str_replace(., "5\\+", "5 \\+"),
  ~ str_trim(., side = "both"),
  ~ str_remove_all(.x, regex("(ye?a??rs?|Ages)", ignore_case = TRUE))
)

print("Reading population projections")
projections <- read_csv(file=fn, col_types = col.types, col_select = c(-migration_scenario_num, -migration_scenario_char))

# move some variables to a combined variable resulting in a long format table.
projections <- projections %>%
  # clean column names
  rename_with(~ gsub("_",".",.x) %>% tolower(), everything()) %>%
  rename(age.num   = age.in.yrs.num,
         age.char  = age.in.yrs.char
  ) %>% 
  mutate(type      = "Projection",
         #age.num   = str_remove(age.char, " yrs?"),# %>%
         age.char  = str_remove(age.char, "yrs?") %>% str_trim("both") %>% str_replace("5\\+","5 \\+"),
         age.iv    = as.age_group(age.char),
         area.name = str_remove(area.name, " County"),
         area.name = recode(.x = area.name, "State of Texas" = "Texas"),
         fips      = paste0("48", str_pad(fips, width = 3, pad = "0", side = "left"))) %>%
  filter(area.name %in% counties) %>% 
  # move some variables to a combined variable resulting in a long format table.
  pivot_longer(cols = total:nh.other.female, 
               names_to = "race.ethnicity", 
               values_to = "population") %>% 
  mutate(race.ethnicity = case_when(
           grepl("(white|black|hispanic|asian|other)\\.total",race.ethnicity) ~ 
             gsub("\\.total","",race.ethnicity),
           TRUE ~ race.ethnicity),
         race.ethnicity = gsub("^nh\\.", "", x = race.ethnicity)
  ) %>%
  separate(col = race.ethnicity,
           c("race.eth","sex"),sep = "\\.", 
           remove=TRUE, 
           fill = "right") %>%
  mutate(sex = ifelse(is.na(sex),"All",sex),
         race.eth = ifelse(race.eth=="total","all",race.eth)) |> 
  select(-age.num)

projections$fips[projections$area.name == "Texas"] <- "48000"
#map(pop %>% select(where(is.character)), unique)

#save(file = paste0(paths$population,"/Projections/Population_Proj_2010-2050.rdata"),pop)
pop_projections <- projections
# arrow::write_parquet(sink = file.path(paths$population, "/Projections/Population_Proj_2010-2050.parquet"), 
#                      x = pop)
rm(projections,fn,col.types)

# ==== Estimates processing, keep only Tarrant, surrounding and comparable counties ====
print("Reading population estimate files since 2010")

read_estimate <- function(.pattern, .counties){
  # get the files to read
  pth   <- file.path(paths$population,"Estimates/Texas Demographic Center/asre")
  files <- list.files(path = pth, pattern = .pattern, full.names = TRUE, )
  
  columns <- cols(County = col_character(),
                  FIPS   = col_character(),
                  Age = col_character(),
                  .default = col_double())
  
  read_and_format <- compose(
    \(df) select(df, -file_name),
    \(df) rename_with(df, 
                      .fn = \(col) str_replace(col, "anglo", "white") |> 
                        str_remove("^nh_"), .cols = everything()),
    \(df) mutate(.data = df, 
                 year   = basename(file_name) |> 
                   str_extract("^20[1-2][0-9]") |> 
                   as.numeric()
                 ),
    clean_names,
    \(f) read_csv(file = f, col_types = columns, id = "file_name")
  )
  ests <- map(files, read_and_format)
  return(ests)
  
}

csvs <- read_estimate(.pattern = "20[1-2][0-9]_ASRE_Estimate_alldata\\.csv", .counties = counties)
csvs[[4]]|> colnames()
estimates <- bind_rows(csvs) |> 
  relocate(year, .after = asian_female)

# save the estimates file to a parquet file for future reading 
asre <- file.path(paths$population, "Estimates/Texas Demographic Center/asre/Estimates.parquet")
write_parquet(x = estimates, file = asre)

pop_estimates <- estimates %>%
  rename_with(.fn = ~str_replace(.x, "_", "."), .cols = everything()) %>% 
  mutate(
    age = process_age_char(age),
    age.iv  = as.age_group(age),
    type = "Estimate",
    fips = paste0("48", fips),
    county = str_to_title(gsub(pattern = " COUNTY", "", county, ignore.case = T)),
    county = recode(.x = county, "State Of Texas" = "Texas")#,
#    migration.scenario.num = NA,
#    migration.scenario.char = NA
  ) %>%
  rename(age.char = age,
         area.name = county) %>%
  filter(area.name %in% counties) %>% 
  pivot_longer(cols     = total:asian.female,
               names_to = "race.sex", values_to = "population") %>%
  separate(col=race.sex ,into = c("race.eth","sex"), sep = "\\.", 
           remove = TRUE, fill = "right") %>%
  mutate(sex = ifelse(sex=="total" | is.na(sex), "All", sex),
         race.eth = ifelse(race.eth=="total","all",race.eth)) %>% 
  filter(! is.na(population))

stopifnot(all(names(pop_estimates) %in% names(pop_projections)))

# ==== Combined population estimates and projections in one data frame and save!===
pop_estimates <- pop_estimates[ ,names(pop_projections)]  # columns in same order
pop_1 <- bind_rows( pop_estimates, pop_projections) |> 
  select(fips, year, area.name, sex, starts_with("age"), race.eth, type, population) |> 
  mutate(race.eth = str_to_title(race.eth))

rm(pop_estimates,pop_projections, estimates, process_age_char)

# ==== Process 2001-2009 county estimates data ------
# estimates were saved from the DSHS Center for Health Statistics as excel files
# Age groups are only available for:
# - single year 0-21,
# - five year age groups up to 85+
# - and total number shown as "all"
print("Processing 2001-2009 population estimate files")
fns <- list.files(pattern = "Dtl200[1-9]{1}.xls", path = paste0(paths$population,"/Estimates"))
fns <- paste(paste0(paths$population,"/Estimates"), fns, sep = "/")
sheets <- map(fns, function(x) read_excel(path = x,
                                          range = "A1:S11731",
                                        col_names = TRUE,
                                        col_types = "text",
                                        #guess_max = 100,
                                        sheet=1))

# make sure all the names in the sheets are the same and in the same order as the first sheet
nms <- names(sheets[[1]])
sheets[3:6] <- map(3:6, function(x) sheets[[x]] %>%
                select(CONAME, YEAR, AGE, CNTY, everything()) %>%
                setNames(.,nm = nms))

sheets[7:9] <- map(7:9, function(x) sheets[[x]] %>%
                     setNames(.,nms))
rm(nms)

# fix the coulmn names to match projection and recent estimate data frame
est01_09 <- bind_rows(sheets) %>%
  mutate_at(.vars = vars(Year, Total:OtherFemale),.funs = as.numeric) %>%
  rename_with(~str_replace(.x,"Anglo", "White"), .cols = contains("Anglo")) %>% 
  rename_with(~str_remove(.x, "Tot") %>% paste0(.,".Total"), 
              .cols = matches(match = "Tot[A-Z]", ignore.case = F))

est_nms <- names(est01_09) %>%
  str_replace_all(., c("Male$" = ".Male", "Female$" = ".Female", "AgeGroup"="Age.Group")) %>%
  str_to_lower(.)

est_nms[est_nms  %in%  c("area" )] <- c("area.name")
est01_09 <- set_names(est01_09,est_nms)

# the fips codes in these older estimate files are actually state ID codes, so
# use this function to  convert to FIPS. 
# The s argument is a character vector that can be
# converted to an integer
convert_2_fips <- function(s){
  ret <- (suppressWarnings(as.integer(s)) * 2 - 1) %>% 
    as.character() %>% 
    str_pad(width = 3, side = "left", pad = "0") %>%
    paste0("48", .)
  return(ret)
}

format_age_group <- compose(
  ~ifelse(nchar(.) == 2, 
          as.character(suppressWarnings(as.integer(.))), 
          .),
  ~recode(., "05-09" = "5-9", "00-04" = "0-4"), 
  ~str_trim(.,side = "both"), 
  ~str_remove_all(.x,"'"))

# create a regular expression of all counties
cnts <- paste0("(",paste(counties,collapse="|"),")")

# add age.num, rename age.groups to age.char
# correct the FIPS codes
est01_09 <- est01_09 %>%
  mutate(age.group  = format_age_group(age.group),
         area.name  = str_to_title(area.name),
         fips       = ifelse(str_detect(area.name, "Texas"), "000", convert_2_fips(fips))
  ) %>%
  filter(area.name %in% counties,
         !age.group %in% c("18+","21+","65+")) %>%
  mutate(
    age.iv          = tarr.population::as.age_group(age.group),
    type            = "Estimate",
    migration.scenario.num  = NA,
    migration.scenario.char = NA) %>%
  rename(age.char   = age.group) %>%
  pivot_longer(cols = total:other.female, names_to = "sex_race", values_to = "population") %>% 
  mutate(sex        = str_extract(sex_race, "(male|female|total)") %>% 
                      str_replace("total", "All"),
         race.eth   =  str_extract(sex_race, "(black|hispanic|other|white)") |> str_to_title()
         ) %>% 
  replace_na(list(race.eth = "All"))

est01_09 <- est01_09[,names(pop_1)]
est01_09$fips[est01_09$fips == "000"] <- "48000"


 
# Final pop data frame and last adjustments to fields
pop <- bind_rows(pop_1,est01_09) |> 
  mutate(sex  = str_to_title(sex),
         year = as.integer(year),
         area.name = factor(x = area.name, levels = counties),
         ethnicity = case_match(.default = "All",
                                race.eth,
                                "Hispanic" ~ "Hispanic"
         ),
         race = case_match(.default = race.eth,
                           race.eth,
                           "Hispanic"  ~ "All"),
         across( c(ethnicity, race), ~factor(.x, levels = sort_values(.x))),
         # ensure age.char is formatted correctly
         age.char  = iv_to_age_group(age.iv) |> 
           fct_reorder(.x = iv_end(age.iv)) |> 
           ordered(),
         across( c(fips, area.name, sex, type), factor)
         ) 

pop <- pop[, c("year", "fips", "area.name", "type", "sex", "age.char", 
               "age.iv", "race", "ethnicity", "population")]

pop <- set_source_url(
  obj = pop, 
  nm  = "Estimates and Projections from the Texas Demographic Center",
  url = "https://demographics.texas.gov/")

rm(fns, sheets, est_nms, convert_2_fips, format_age_group, pop_1, est01_09,cnts)
