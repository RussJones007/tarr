# testing th correct_city function
library(dplyr)
library(stringr)
fn <- file.path("data-raw/cities.rds")
cities <-  readRDS(fn) 

city_fn <- file.path("~/R/Projects/R Spatial/Tarrant/Cities/Cities_2013.shp")
cities_geo <- sf::st_read(dsn = city_fn)
plot(cities_geo)
city_names <- cities_geo$CITY_NAME %>% 
  unique() %>% 
  sort() %>% 
  stringr::str_to_title()
dput(city_names)
rm(city_fn, fn, cities_geo)


#======  correct city 2 devlopment ====
city_proc <- cities %>% 
  stringr::str_to_title() %>% 
  # remove trailing comma and anything after that
  str_extract( "^[^,]*") %>% 
  gsub(" Tx","",.) %>% 
  gsub("(^No.|^N )","North",.) %>% 
  gsub("^Ft.?","Fort",.)

# score the distance between the city_proc (left column) and city_names (top row)
distances <- adist(city_proc, city_names)

setNum <- function(num_vec){
  nm <- min(num_vec)
  nm <- ifelse(nm < 3, nm, NA)
}

# gets the minimum score, if it is above the threshold then returns NA
mini <- apply(distances, 1, setNum)

return_city <- function(ndx){
  if(is.na(mini[ndx]))
    ret <- city_proc[ndx]
  else {
    ret <- city_names[which(distances[ndx,] == mini[ndx])]
    ret <- ifelse(length(ret)>1,ret[1], ret)
  }
  return(ret)
}
return_city(632)

city_cor <- purrr::map(seq_along(mini), return_city) %>% 
  unlist()

multi <- purrr::map(city_cor, function(x)
  length(x) > 1) %>% 
  unlist()
city_cor[multi]
city_proc[multi]

tmp <- cbind(cities, city_cor)

which(distances[6,] == mini[6])
city_names[17]
mini
which()


