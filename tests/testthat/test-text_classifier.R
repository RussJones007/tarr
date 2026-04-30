test_that("make_classifier classifies text correctly", {
  clf <- make_classifier(
    true_patterns = c(first_yes = "foo", second_yes = "bar"),
    false_patterns = c(first_no = "qux", second_no = "zap")
  )
  
  expect_identical(
    clf(c("foo", "qux", "foo qux", "none")),
    c(TRUE, FALSE, NA, NA)
  )
})

capture_warnings <- function(expr) {
  warnings <- character()
  value <- withCallingHandlers(
    expr,
    warning = function(w) {
      warnings <<- c(warnings, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )
  
  list(value = value, warnings = warnings)
}

test_that("make_classifier respects ignore_case", {
  insensitive <- make_classifier(
    true_patterns = c(match = "foo"),
    false_patterns = character()
  )
  sensitive <- make_classifier(
    true_patterns = c(match = "foo"),
    false_patterns = character(),
    ignore_case = FALSE
  )
  
  expect_identical(insensitive("FOO"), TRUE)
  expect_identical(sensitive("FOO"), NA)
})

test_that("detail output reports first matching pattern in vector order", {
  clf <- make_classifier(
    true_patterns = c(first_yes = "foo", second_yes = "foo bar", grouped = "(baz)"),
    false_patterns = c(first_no = "qux", second_no = "zap")
  )
  
  detail <- clf(c("foo bar", "baz", "qux", "none"), return_detail = TRUE)
  
  expect_named(
    detail,
    c("text", "classification", "matched_true_pattern", "matched_false_pattern")
  )
  expect_identical(detail$classification, c(TRUE, TRUE, FALSE, NA))
  expect_identical(
    detail$matched_true_pattern,
    c("first_yes", "grouped", NA_character_, NA_character_)
  )
  expect_identical(
    detail$matched_false_pattern,
    c(NA_character_, NA_character_, "first_no", NA_character_)
  )
})

test_that("pattern names are normalized with warnings and blanks become sequential labels", {
  pats <- stats::setNames(c("foo", "bar"), c("Call Back", ""))
  
  expect_warning(
    clf <- make_classifier(pats, character()),
    regexp = 'Pattern names were normalized: "Call Back" -> "call_back", "" -> "p2"'
  )
  
  expect_identical(
    classifier_patterns(clf),
    list(
      true = c(call_back = "foo", p2 = "bar"),
      false = character()
    )
  )
})

test_that("duplicate names after normalization error with a useful message", {
  expect_error(
    suppressWarnings(
      make_classifier(
        c("Call Back" = "foo", "call-back" = "bar"),
        character()
      )
    ),
    regexp = 'Pattern names must be unique after normalization\\. Duplicates: "call_back"\\. Rename the duplicated entries explicitly\\.'
  )
})

test_that("classifier_patterns returns base and effective merged patterns", {
  clf <- make_classifier(
    true_patterns = c(base_yes = "yes"),
    false_patterns = c(base_no = "no")
  )
  
  expect_identical(
    classifier_patterns(clf),
    list(
      true = c(base_yes = "yes"),
      false = c(base_no = "no")
    )
  )
  
  captured <- capture_warnings(
    classifier_patterns(
      clf,
      effective = TRUE,
      extra_true = c("Call Back" = "followed up"),
      extra_false = c("Other No" = "declined")
    )
  )
  
  effective <- captured$value
  expect_true(any(grepl('"Call Back" -> "call_back"', captured$warnings, fixed = TRUE)))
  expect_true(any(grepl('"Other No" -> "other_no"', captured$warnings, fixed = TRUE)))
  
  expect_identical(
    effective,
    list(
      merged_true = c(base_yes = "yes", call_back = "followed up"),
      merged_false = c(base_no = "no", other_no = "declined")
    )
  )
})

test_that("classifier_patterns effective extras reject duplicate names after normalization", {
  clf <- make_classifier(
    true_patterns = c(alpha = "yes"),
    false_patterns = character()
  )
  
  expect_error(
    suppressWarnings(
      classifier_patterns(
        clf,
        effective = TRUE,
        extra_true = c("Call Back" = "foo", "call-back" = "bar")
      )
    ),
    regexp = 'Pattern names must be unique after normalization\\. Duplicates: "call_back"\\. Rename the duplicated entries explicitly\\.'
  )
})

test_that("classifier_patterns replacement updates classifier behavior and detail labels", {
  clf <- make_classifier(
    true_patterns = c(alpha = "foo"),
    false_patterns = c(beta = "bar")
  )
  
  captured <- capture_warnings(
    `classifier_patterns<-`(clf, list(
      true = c("Call Back" = "alpha", "123" = "beta"),
      false = c("Other No" = "gamma")
    ))
  )
  clf <- captured$value
  expect_true(any(grepl('"Call Back" -> "call_back"', captured$warnings, fixed = TRUE)))
  expect_true(any(grepl('"123" -> "p_123"', captured$warnings, fixed = TRUE)))
  expect_true(any(grepl('"Other No" -> "other_no"', captured$warnings, fixed = TRUE)))
  
  detail <- clf(c("alpha", "beta", "gamma"), return_detail = TRUE)
  
  expect_identical(detail$classification, c(TRUE, TRUE, FALSE))
  expect_identical(detail$matched_true_pattern, c("call_back", "p_123", NA_character_))
  expect_identical(detail$matched_false_pattern, c(NA_character_, NA_character_, "other_no"))
})

test_that("runtime extras participate in classification and detailed pattern reporting", {
  clf <- make_classifier(
    true_patterns = c(alpha = "foo"),
    false_patterns = character()
  )
  
  expect_warning(
    detail <- clf(
      c("followed up", "foo"),
      return_detail = TRUE,
      extra_true = c("Call Back" = "followed up")
    ),
    regexp = 'Pattern names were normalized: "Call Back" -> "call_back"'
  )
  
  expect_identical(detail$classification, c(TRUE, TRUE))
  expect_identical(detail$matched_true_pattern, c("call_back", "alpha"))
})

test_that("case_interview_classifier smoke test returns expected output structure", {
  expect_no_warning({
    detail <- case_interview_classifier(
      c("Interview completed with patient", "Left voicemail for nurse"),
      return_detail = TRUE
    )
  })
  
  expect_s3_class(detail, "data.frame")
  expect_named(
    detail,
    c("text", "classification", "matched_true_pattern", "matched_false_pattern")
  )
  expect_equal(nrow(detail), 2L)
})
