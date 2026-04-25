# Test the geocode file functions


# correct_city() testing
test_that("Correction of cities", {
  nms      <- c("ftw", "forth worth", "NRH", "Noth RIchland Hills", "ALZE",
                   "dallis", "badfird", "Dalworth grdns", "arlyngton")
  real_nms <- c("Fort Worth", "Fort Worth", "North Richland Hills", "North Richland Hills", "Azle",
                   "Dallas", "Bedford", "Dalworthington Gardens", "Arlington")
  expect_true( all( correct_city(.city = nms)  %in% real_nms))
}
)
