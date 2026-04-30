# test-Age_calculation.r
# 
# Tests for the age calculation and categorization functions
# Created April 26, 2023
# R. Jones


test_that(
  desc = "Age calculation",
  code = {
    # set up variables to test
    ages <- 82:0 |> 
      set_units("year")
    
    birth_dates <- seq(lubridate::mdy("04-14-1960"), lubridate::mdy("04-14-2042"), by =  "year") 
    end_dates   <- max(birth_dates) + 1
    end_date_days_add <- c(1,3,5,7,9) |> 
      set_units("day")
    end_dates_days <- birth_dates[1:length(end_date_days_add)] + as.integer(end_date_days_add)
    day_ages <- age_calc(start = birth_dates, end_dates, unit = "day") 
    expect_s3_class(day_ages, "units")
    expect_equal(units(day_ages)$numerator, "d")
    expect_error(age_calc("04-14-1960"), "is.date")
    expect_equal(units(age_calc(as.Date("1960-04-14")))[["numerator"]], "year")
    expect_equal(age.calc(.start = birth_dates, .end = end_dates), ages)
    expect_equal(age_calc(birth_dates, end_dates), ages)
    expect_length(age_calc(birth_dates, end_dates), length(birth_dates))
    expect_type(age_calc(birth_dates, end_dates), "double")
    expect_equal(object = attr(age_calc(birth_dates[1:5], unit = "day"), which = "units")$numerator, 
                 expected = "d")
    expect_equal(age_calc(birth_dates[1:length(end_date_days_add)],
                          end_dates_days, unit = "day"), end_date_days_add)
  }
)


test_that(
  desc = "Age categorization",
  {
    ages <- 82:0 |> 
      set_units("year")
    ann_cat    <- age_cat(ages = ages, by = age.groups$Annual)
    ann_table  <- ann_cat |> table()
    ann_cat2   <- age_cat(ages = ages, by = 10)
    
    expect_equal(levels(ann_cat) |> length(), 5)
    expect_equal(attr(ann_cat, "units"), "year")
    expect_equal(ann_table |> as.numeric(), c(15, 10, 20, 20, 18))
    expect_equal((ann_table |> dimnames())[[1]], c("0-14", "15-24", "25-44", "45-64", "65 +"))
  }
)

