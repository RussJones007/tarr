#synthetic.r
# create a synthetic data set with lat, lon, birth dates, onset dates, conditions, race, and ethnicity,
library(tidyverse)
library(lubridate)
library(sf)
library(lubridate)
library(outbreaks)

# number of records

# function to generate cases from other outbreaks and place them in Tarrant
# all parameters except df are passed as quoted
# @param disease_name is the name of the condition.
# @param df is the data frame
# @param dob,onset are dates.  If age is present it is used to calculate the dob from the onset,
# other wise dob is shifted to more recent dates
# @param age,lat,lon are numeric 
# @param sex,race are factors or character
# @returns a dataframe with same number of cases but moved artifically to Tarrant Couty
synthetic_df <- function(disease_name, df, age, sex, race, dob,  onset, outcome, lat, lon) {
  stopifnot(! missing(onset))
  stopifnot(! missing(disease_name))
  
  # ret is 
  ret <- list()     # the dataframe to return
  rows <- nrow(df)  # number of rows to return in ret
  
  sim_sample <- partial(sample, size = rows, replace = TRUE) # simplified sample function
  
  ret$condition <- rep(disease_name, rows)
   # add sex 
   if(missing(sex)){
     ret$sex <- sim_sample(x = c("male", "female", "unknown"), prob = c(0.49, 0.50, 0.01))
   } else {
     ret$sex <- df[[sex]] |> as.character()
   }
   if(! missing(outcome)) {
     ret$outcome <- df[[outcome]] |> as.character()
   }
   
   # add race
   race_probs <- c(
     "asian"            = 0.02, 
     "black"            = 0.128, 
     "native american"  = 0.06, 
     "pacific islander" = 0.0916, 
     "white"            = .712, 
     "other"            = 0.116)
   if(missing(race)) {
     ret$race <- sim_sample(x = names(race_probs), prob = race_probs)
     } else {
       ret$race <- df[[race]] |> as.character()
     }
   rm(race_probs)
   
  # shift onset to 2020 and later
   min_onset  <- min(df[[onset]], na.rm = TRUE)
   diff_onset <- mdy("01-01-2020") - min_onset
   ret$onset  <- df[[onset]] + diff_onset
   rm(min_onset)
   
   # set up date of birth using age if available, otherwise shift dob by diff_onset 
  if(! missing(age)) {
    ret$dob <- df[[onset]] - lubridate::years(df[[age]])
  } else if(! missing(dob)) {
    ret$dob <- df[[dob]] + diff_onset
  } else {
    mean_age <- sample(x = 25:50, size = 1)
    sd_age   <- sample(x = 7:20, size = 1)
    ages <- sim_sample(x = rnorm(n = rows, mean = mean_age, sd = sd_age)) |> as.integer()
    ret$dob <- df[[onset]] - lubridate::years(ages)
    rm(mean_age, sd_age, ages)
  }
   
  # shift coordinates to tarrant area, assuming CRS 4326 
   border_fn <- file.path(paths$spatial, "Tarrant/Border/Tarrant Border.rds")
   border    <- readRDS(border_fn)
   st_crs(border) <- st_crs(border)
   box <- st_bbox(border)
   
  if(! (missing(lat) & missing(lon))){
    stopifnot(all(map_lgl(c(lat,lon), ~ df[[.x]] |> is.numeric())))
    ret$lat <- scales::rescale(df[[lat]], to = c(box["ymin"], box["ymax"]))
    ret$lon <- scales::rescale(df[[lon]], to = c(box["xmin"], box["xmax"]))
  }  else {
    # assign random coordinates
    ret$lon = runif(n = rows, min = box["xmin"], max = box["xmax"])
    ret$lat = runif(n = rows, min = box["ymin"], max = box["ymax"]) 
  }
  rm(border, box, border_fn) 
   
  ret <- compact(ret) |> bind_cols()
  return(ret)
}

#undebug(synthetic_df)

#mers_korea_2015$linelist |> glimpse()
fooflu <- synthetic_df(disease_name = "fooflu", 
                    outcome = "outcome",
                    df = mers_korea_2015$linelist, 
                    age = "age", 
                    onset = "dt_onset")

#ebola_sim$linelist |> glimpse()
lorebiasis <- synthetic_df("lorebiasis", 
                           df = ebola_sim$linelist,
                           sex = "gender", 
                           outcome = "outcome",
                           onset = "date_of_onset",
                           lon = "lon", 
                           lat = "lat")

synthetic_outbreak <- bind_rows(fooflu, lorebiasis) |> 
  mutate(sex = case_match(
    sex,
    .default = sex,
    c("m", "male")   ~ "male",
    c("f", "female") ~ "female"),
    outcome = case_match(
      outcome,
      .default = outcome,
      "Alive" ~ "Recover",
      "Dead"  ~ "Death")
    )
rm(fooflu, lorebiasis, synthetic_df)

