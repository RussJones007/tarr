
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

test_that("is_yn recognizes and normalizes yes/no codes", {
  result <- is_yn(c("YES", "nO", "Unknown", NA))

  expect_true(isTRUE(result))
  expect_identical(
    attr(result, "values"),
    list(true_code = "yes", false_code = "no", unk_code = "unknown")
  )

  numeric_result <- is_yn(c(0, 1))
  expect_true(isTRUE(numeric_result))
  expect_identical(
    attr(numeric_result, "values"),
    list(true_code = "1", false_code = "0", unk_code = character())
  )
})

test_that("is_yn supports synonyms and observed factor values", {
  result <- is_yn(c("y", "yes", "n", "no", "u", "unk"))
  expect_true(isTRUE(result))
  expect_identical(
    attr(result, "values"),
    list(
      true_code = c("y", "yes"),
      false_code = c("n", "no"),
      unk_code = c("u", "unk")
    )
  )

  factor_result <- is_yn(factor(c("Yes", "No"), levels = c("Yes", "No", "invalid")))
  expect_true(isTRUE(factor_result))
})

test_that("is_yn rejects non-yes/no vectors", {
  expect_false(is_yn(logical()))
  expect_false(is_yn(c(NA_character_, NA_character_)))
  expect_false(is_yn(c("unknown", "unk")))
  expect_false(is_yn(c("yes", "maybe")))
})

test_that("yn_2_logical converts normalized yes/no categories", {
  expect_identical(
    yn_2_logical(c("y", "yes", "TRUE", "n", "No", "false")),
    c(TRUE, TRUE, TRUE, FALSE, FALSE, FALSE)
  )
  expect_identical(yn_2_logical(c(1, 0, 1)), c(TRUE, FALSE, TRUE))
})

test_that("yn_2_logical preserves unknown and missing values as NA", {
  result <- yn_2_logical(c("yes", "unknown", "UNK", "", NA, "no"))

  expect_identical(result, c(TRUE, NA, NA, NA, NA, FALSE))
})

test_that("yn_2_logical handles factors and invalid input", {
  value <- factor(
    c("Yes", "No", "Unknown", NA),
    levels = c("Yes", "No", "Unknown", "unused")
  )

  expect_identical(yn_2_logical(value), c(TRUE, FALSE, NA, NA))
  expect_null(yn_2_logical(c("yes", "maybe")))
  expect_null(yn_2_logical(c(TRUE, FALSE)))
})

