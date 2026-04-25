
# Test the simplify_term function ---------------------------------------------------------------------------------

test_that("Use of simple_conditions vector", {
  test  = c("West Nile Fever", "West Nile Virus, Non-neuroinvasive", "West Nile Virus Non-neuroinvasive disease", 
            "Zika rash", "Zika virus infection, non-congenital", "Zika disease, non-congenital", "Zika headache")
  result = simplify_term(test, simple_conditions)
  expect_length(result, length(test))
  expect_true( str_detect( result, "West Nile non-neuroinvasive") |> sum() == 3)
  test_conditions <- c(simple_conditions, "Zika\\s*." = "Just a rash" )
  result = simplify_term(test, test_conditions)
  result
  expect_length(result, length(test))
  expect_contains(result, "Just a rash")
})

