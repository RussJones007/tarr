# Test Correct_city2 ------------------------------------------------------
# Test the correct city function
library(tarr)
library(dplyr)
library(stringr)
#rm(list = ls())
# test data set
cities <- readRDS(file.path("data-raw/cities.rds"))

system.time(
  tmp <- correct_city(cities)
)
tmp %>% table()
city.names.outside %>% sort()
# number of unique citites before and after correct_city
purrr::map(list(cities, tmp), ~unique(.) %>% length())

cmp <- cbind(cities, tmp) %>%
  as_tibble() %>%
  dplyr::arrange(tmp) %>%
  distinct(cities, tmp)

cmp %>% View

city_proc <- cities %>%
  str_to_title() %>%
  str_extract( "^[^,]*") %>%
  gsub("( Tx| Texas| To)","",.) %>%
  unique()

mtr <- agrep("Fort Worth", x = city_proc, ignore.case = T, fixed = F)
city_proc[mtr]
city_proc %>% unique() %>% length()



# Function testing -------------------------------------------------------------

correct_city <- function(.city, .threshold = 2){
  .city = cities
  .threshold = 2
  
  city_proc <- .city %>%
    # standardize the city names to title case
    stringr::str_to_title() %>%
    # remove trailing comma and anything after that, e.g. ", Tx"
    str_extract( "^[^,]*") %>%
    gsub("(,? Tx\\.?$| Texas$| To$)","",.) %>% 
    # Remove "Other -
    gsub("Other ?-?", "", .) %>% 
    str_trim(side = "both")
  
  # Expand or replace common abbreviations, use title case
  acryn <- c("^No\\.? "          = "North ",
             "^N\\.? "           = "North ",
             "^Ft\\.? "          = "Fort ",
             "^Forth"            = "Fort",
             "^Ftw"              = "Fort Worth",
             "^Fw$"              = "Fort Worth",
             "^Arl$"             = "Arlington",
             "^N.?r.?h$"         = "North Richland Hills",
             "^North Richland"   = "North Richland Hills",
             "Richland H\\.?i?$" = "Richland Hills",
             " Grdns?$"          = " Gardens",
             " Hls$"             = " Hills",
             " Vlg$"             = " Village"
  )
  
  city_proc <- city_proc %>%
    stringr::str_replace_all(pattern = acryn)
  rm(acryn)
  
    # remove words or letters in the beginning that confuse the fuzzy changes
  # especially for Azle
  removals <- c("(^ARLY$|^AT$|^Bed$|^ARLIY$|^Drive$|^GV$|^NEH$|^RD$|^APT$)")
  city_proc <- city_proc %>% 
    {stringr::str_remove(str_to_title(.), pattern = str_to_title(removals))}
  
  
  # cities to search are from these character vectors
  all_cities <- city.names
  all_cities <- c(city.names, city_names_outside)
  
  
  # local function to find the minimum score equal to or less than .threshold for a
  # vector of numbers.  If it is above limit (.threshold), then returns NA which
  # signals no match found.
  setNum <- function(num_vec, limit){
    nm <- min(num_vec)
    nm <- ifelse(nm <= limit, nm, NA)
  }
  
  # local function to return the fuzzy matched city or the original city for those
  # above the threshold as set in setNum.  When two cities match, the first one is used
  return_city <- function(n){
    if(is.na(mini[n]))
      #ret <- city_proc_sub[n]
      ret <- city_proc_sub[n]
    else {
      ret <- city.names[which(distances[n,] == mini[n])]
      ret <- ifelse(length(ret)>1,ret[1], ret)
    }
  }
  
  # --- Using word distance (adist) to match city names
  # Many city names are correct and will not need to go through subsequent spelling
  # checks.  So use a subset of the city_proc for further cleaning. ndx are the
  # indices of  city names that do not match and must be processed further. Finally
  # ndx is used to save the processed city names to the city_proc vector that is
  # returned from the function.
  # Cities with shorter names tend to match other shorter names rather easily.
  # Therefore those with 5 or fewer characters are matched using a lower threshold.
  # The all_cities variable are the reference vector of cities for matching that is 
  # made it in the package.
  
  # match lower character length cities 
  compare_city <- all_cities[nchar(all_cities) <= 5]
  small_ndx <- which(nchar(city_proc) <= 5)
  city_proc_sub <- city_proc[small_ndx]
  small_distances <- adist(x = city_proc_sub, y = compare_city)
  mini <- apply(small_distances, 1, setNum)
  
  
  
  ndx <- which(!city_proc %in% all_cities 
               & !is.na(city_proc)  
               #& (nchar(city_proc) > 4)
               )
  city_proc_sub <- city_proc[ndx]
  
  # use word distances to to determine the best match for cities.
  # score the distances between city names to obtain a matrix
  distances <- adist(x = city_proc_sub, y = all_cities)
  
  # get the minimum score for each row of the matrix
  mini <- apply(distances, 1, setNum)
  
  # iterate through all the result scores in mini and call return_city
  city_proc_sub <- purrr::map_chr(seq_along(mini), return_city)
  
  #--- Use agrep pattern matching to get a matched city,
  for(i in seq_along(city.names)){
    city_proc_sub[agrep(pattern = paste0("^",city.names[i]),city_proc_sub, fixed = F)] <- city.names[i]
  }
  # save the newly matched cities
  city_proc[ndx] <- city_proc_sub
  
  return(city_proc)
}

