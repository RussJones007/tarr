# as.age_group function test------
test_that(
  "as.age_group using character value",
  {
    # character values to use in testing
    ages     <- c("< 5",  "10-14", "15 - 19", "20 to 24", "25 +", "65 >", "30", "45 thru 50", 
                  "32 through 38") 
    expect_no_condition(as.age_group(ages))  
    age_gr <- as.age_group(ages)
    expect_s3_class(age_gr, "age_group")
    expect_length(age_gr, 9)
    bad_age    <- c("1-4", "0 except 4", "1 to 4", "1thru4", "2 and 4")
    expect_error(as.age_group(bad_age), regexp = "These age groups in x are malformed:")
    valid <- valid_age_group_symbol(bad_age)
    expect_false(valid)
    expect_contains(get_locations(valid), c(2,5))

    # testing for single ages plus age groups in the same vector
    ages <- c("0", "1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13", "14", "15", "16", "17", "18", "19", 
              "20", "21", "22", "23", "24", "25", "26", "27", "28", "29", "30", "31", "32", "33", "34", "35", "36", "37", 
              "38", "39", "40", "41", "42", "43", "44", "45", "46", "47", "48", "49", "50", "51", "52", "53", "54", "55", 
              "56", "57", "58", "59", "60", "61", "62", "63", "64", "65", "66", "67", "68", "69", "70", "71", "72", "73", 
              "74", "75", "76", "77", "78", "79", "80", "81", "82", "83", "84", "85", "86", "87", "88", "89", "90", "91", 
              "92", "93", "94", "95", "96", "97", "98", "99", "100-104", "105-109", "110 +", "All")
    
    ages_iv <- as.age_group(ages)
    expect_length(ages_iv, length(ages))
    expect_s3_class(ages_iv, "age_group")
    age_char <- age_group_to_char(ages_iv)
    age_char2 <- as.character(ages_iv)
    expect_equal(age_char, age_char2)
    expect_equal(ages, age_char)
  })



test_that(
  desc = "as.age_group using numeric values",
  code = {
    age_gr <- c(0, 5, 10, 15, 20, 25, 65, Inf)
    exp_age_gr <- ivs::iv_pairs( c(0, 5), c(5, 10), c(10, 15), c(15, 20), c( 20, 25),   c(25, 65), c(65, Inf))
    grp <- as.age_group(x = age_gr)
    expect_s3_class(grp, "age_group")
    expect_s3_class(grp, "ivs_iv")
    expect_length(grp, length(exp_age_gr))
    expect_equal( as.age_group(x = age_gr) |> vctrs::vec_data(), exp_age_gr |> vctrs::vec_data())
    expect_false(as.character(grp) |> str_detect("NA") |> all())
    as.character(grp)
    format(grp)
})

test_that(
  desc = "Testing the age_group class constructor and S3 functions",
  code = {
    base_grp <- c(" 0-4",  "10-14", "15-19" , "20-24", "25-30", "35-39", "40-44", "45-49", "50-54", "55-59", "60 +")
    ageGrp <-as.age_group(base_grp)
    expect_s3_class(ageGrp, "age_group")
    expect_length(ageGrp, 11)
    expect_identical(attr(ageGrp, which = "symbols")$separator, "-")
    expect_identical(attr(ageGrp, which = "symbols")$below, "0-")
  
  }
)

test_that("Age group to character", {
  base_grp <- c("< 2", "2-4", "5-9", "10-14", "15-19" , "20-24", "25-30", "35-39", "40 +")
  age_grps <- as.age_group(base_grp)
  expect_length(age_grps, length(base_grp))
  expect_s3_class(age_grps, "age_group")
  age_grps_char <- as.character(age_grps)
  expect_equal(base_grp, age_grps_char)
  
  tmp0 <- c(0:5) |> as.age_group()
  expect_equal(iv_start(tmp0), 0:5)
  expect_equal(as.character(tmp0), c("0", "1", "2", "3", "4", "5"))
  
  tmp1 <- c("0-2", "3-4", "5-9", "10-14", "15-19", "20-24", "25-30", "35-39", "40+") |> 
    as.age_group()
  expect_equal(as.character(tmp1), c("0-2", "3-4", "5-9", "10-14", "15-19", "20-24", "25-30", "35-39", "40+"))
 
}
)

test_that(
  "iv to age_group",
  {
    iv_grp <- ivs::iv_pairs( c(0, 5), c(5, 10), c(10, 15), c(15, 20), c( 20, 25),   c(25, 65), c(65, Inf))
    grp <- as.age_group(iv_grp)
    #grp
    expect_s3_class(grp, "age_group")
    expect_length(grp, length(iv_grp))
  })

test_that(
  "Testing age_group symbol attributes",
  {
    base_grp <- c(0, 5, 10, 15, 20, 25, 65)
    grp <-as.age_group(base_grp)
    syms <- get_symbols(grp)
    expect_identical(syms$below, "0-")
    expect_identical(syms$sep, "-")
    expect_identical(syms$above, "+")
    rm(syms)
    expect_error(as.age_group(base_grp, below = "< ", sep = " to ", above = " and above"), "These age groups in x are malformed:")
    grp2 <- as.age_group(base_grp, below = "< ", sep = " to ", above = " above")
    as.character(grp2)
    as.character(grp2) |> as.age_group()
    format(grp2)
    syms2 <- get_symbols(grp2)
    expect_identical(syms2$below, "< ")
    expect_identical(syms2$sep, " to ")
    expect_identical(syms2$above, " above")
  }
)

test_that(
  "Testing symbol validity and the valid_age_group_symbol",
  {
    valid_syms <-c("through", "thru", "to", " -", "-", ",", ">", "+", "all", "total", "plus", "above", ",")
    res <- valid_age_group_symbol(valid_syms)
    expect_true(res)
    expect_null(get_problems(res))
    invalid_syms <- c("thru", "from here", "nope", "+", "degrees F", " <", ";", "&", "this above", "65", "12-15")
    res <- valid_age_group_symbol(invalid_syms)
    get_problems(res)
    get_locations(res)
    expect_false(res)
    expect_contains(get_problems(res), c("from here", "nope", "degrees F", ";", "&", "this above"))
  }
)

test_that(
  "Casting age_group to other types",
  {
    base_grp <- c(0, 5, 10, 15, 20, 25, 65)
    grp <- as.age_group(base_grp)
    int_age  <- as.integer(grp)
    expect_equal(int_age, base_grp)
    char_age <- as.character(grp)
    expect_equal(as.age_group(int_age), grp)
    #undebug(as.factor)
    fac_age  <- as.factor(grp)
    ord_age  <- as.ordered(grp)
    expect_true(is.ordered(ord_age))
    expect_length(grp, 7)
  }
)

test_that(
  "Sorting age_group tests",
  {
    # group that isnot in order
    iv_grp     <- ivs::iv_pairs( c(0, 5), c(10, 15), c(15, 20), c(5, 10),  c( 20, 25), c(25, 65), c(65, Inf))
    sorted_grp <- ivs::iv_pairs( c(0, 5), c(5, 10),  c(10, 15), c(15, 20), c( 20, 25), c(25, 65), c(65, Inf))
    expect_false(all(iv_grp == sorted_grp))
    expect_equal(sort(iv_grp), sorted_grp)
    
    # test the age_group class
    iv_grp     <- as.age_group(iv_grp)
    sorted_grp <- as.age_group((sorted_grp))
    expect_equal(sort(iv_grp), sorted_grp)
    
  }
)


