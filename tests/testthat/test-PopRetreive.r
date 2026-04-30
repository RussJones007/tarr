# test-PopRetrieve.r

test_that(
  desc = "Testing retrieve_county_population using census data",
  code = {
    df <- retrieve_county_population(.pop_df = population$census, .year = 2020)
    expect_length(df, 9)  # number of columns
    expect_equal( sum( map_lgl(df, is.factor)), 6)  # six factor variables present
    expect_length( levels(df$age.char), 1)  # only one level
    expect_equal( df$population, 2110640)   # defaults should result in population for Tarrant 

    df <- retrieve_county_population(.pop_df = population$census, .year = 2020, .age.groups = age.groups$Yr.10) 
    expect_length( levels(df$age.char), 8)  # levels with age group set at 10 years
    expect_equal( df$area.name |> unique() |> as.character(), "Tarrant")  # default for county should be Tarrant
    expect_equal( nrow(df), 8)             # one record for each age group
    expect_equal( sum(df$population), 2110640)  #  population sum is correct
    
    df <- retrieve_county_population(population$census.estimates, .year = 2020, 
                                     .age.groups = age.groups$Yr.5)
    expect_equal( levels(df$age.char), 
                  c("< 5", "5-9", "10-14", "15-19", "20-24", "25-29", "30-34", 
                    "35-39", "40-44", "45-49", "50-54", "55-59", "60-64", "65-69", 
                    "70 +"))
    df <- retrieve_county_population(.pop_df = population$census, .year = 2020, 
                                     .age.groups = age.groups$Yr.10,
                                     .ethnicity = c("Hispanic", "Non-Hispanic"),
                                     .sex = c("Female", "Male")
    )
    expect_equal(with( df, sum(population[sex == "Female" & ethnicity == "Hispanic"])), 310433)
    expect_length(levels(df$sex), 2)
    expect_length(levels(df$age.char), 8)
    expect_length(levels(df$ethnicity), 2)
  })




