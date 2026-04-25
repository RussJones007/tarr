#experiment.r

# experiment creating an array from  a population data frame
# array manipulation is much faster than dplyr on a data frame
# Just need to figure out how to do age groups
p <- population$census |> 
  select(-fips, -age.iv) |> 
  filter(#area.name == "Tarrant", 
         if_all((sex:ethnicity), ~ .x != "All")) |> 
  droplevels() |> 
  arrange(year, area.name, sex, age.char, race, ethnicity) |> 
  as.data.frame()

str(p)

# To create an array from  a tidy data frame, the data frame should have the same sort orders
# must order each data variable, then call array with each dimension
# using the reversed order of the ordering.  For example ordering by year, age, and sex, then to pass to 
a <- array(data = p$population, 
           dim = c(
             length(unique(p$year)),
             length(unique(p$ethnicity)),
             length(unique(p$sex)),
             length(unique(p$race)),
             length(unique(p$area.name)),
             length(unique(p$age.char))
           ),
           dimnames = list(
             year = unique(p$year),
             eth  = unique(p$ethnicity),
             sex  = unique(p$sex),
             race = unique(p$race),
             area = unique(p$area.name),
             age  = unique(p$age.char)
           )
)
str(a)
class(a)
dim(a)
attributes(a)
attr(a, "age_iv") <- dimnames(a)[["age"]] |> as.age_group.character()
attr(a, "age_iv") |> class()
attr(a, "age_iv") |> length()
dimnames(a) |> names()

races <- c("American Indian And Alaska Native", "Asian", "Black", "Hawaiian Or Pacific Islander", "Other", "Two Or More", "White" )
eths <- c("Hispanic", "Non-Hispanic")
tmp <- a[year = "2020", eth = eths, sex = c("Male", "Female"), race = races, area= c("Tarrant"), 
         age = as.character(0:99)]

attributes(tmp)
# tmp <- a[eth = "Hispanic" ,race = "Black",
#          age = c("0-4", "5-9", "10-14", "15-19"), 
#          sex = c("Male", "Female"), area= "Tarrant", year = "2020"]
dim(tmp)
dimnames(tmp) 
tmp[, , race = "Black" ,] |> as.data.frame()
df <- tmp |> as.data.frame()
population$census.estimates$age.char |> unique()

p |> 
  filter(ethnicity == "Hispanic",
         year == 2020) |> 
  pull(population) |> 
  sum()

             
array_method <- function(a = a) {
  tmp <- a[eth = c("Hispanic") ,race = c("Black", "White") ,as.character(10:20) , sex = "Male", "Tarrant", year = "2020"] 
  #Total <-  apply(tmp, 2, sum)
  Total <- colSums(tmp)
  tmp2 <- rbind(tmp, Total) 
  dimnames(tmp2) <- list(Race = dimnames(tmp2)[[1]], Age = dimnames(tmp2)[[2]])
  th <- tmp[1,] |> names()  |> as.age_group()  # used ot see how much as,age_group affects timing
  tmp2
}

array_method(a)


df_method <- function(p = p){
  sel <- p |> 
    filter(year == 2020, area.name == "Tarrant", ethnicity == "Hispanic",
           sex == "Male", age.char  %in% as.character(10:20), race  %in%  c("Black", "White") ) |> 
    select(race, age.char, population) #|> 
    #pivot_wider(names_from = age.char, values_from = population) 
  sel
  # total <- sel |> 
  #   group_by(age.char) |> 
  #   reframe(race = "Total",
  #           population = sum(population)) 
  # 
  # ret <- bind_rows(sel, total) |> 
  #   pivot_wider(names_from = age.char, values_from = population) 
  #ret
}

df_method(p)
microbenchmark::microbenchmark(
  arr = array_method(a),
  df  = df_method(p)
)
#usethis::edit_r_profile()
paths


# Calculate the non-hispanic numbers for Demographic Center Data --------------------------------------------------
population_doc
e <- population$texas.estimates
attributes(e)
e$ethnicity |> unique()

e_wide <- e |> 
  pivot_wider(names_from = ethnicity, values_from = population)
