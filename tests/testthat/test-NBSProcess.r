test_that("nbs_import caches only default filtered results", {
  source_file <- file.path(withr::local_tempdir(), "nbs_export.csv")
  utils::write.csv(data.frame(value = 1), source_file, row.names = FALSE)
  cache_file <- sub("\\.csv$", ".parquet", source_file)

  writes <- 0L
  include_all_values <- logical()

  testthat::local_mocked_bindings(
    nbs_process = function(.df, .include.all = FALSE, .report_type = NULL) {
      include_all_values <<- c(include_all_values, .include.all)
      data.frame(value = if(.include.all) "all" else "filtered")
    },
    write_parquet = function(x, file) {
      writes <<- writes + 1L
      nanoparquet::write_parquet(x, file = file)
    },
    .package = "tarr"
  )

  filtered <- nbs_import(
    .file = source_file,
    .include.all = FALSE,
    .use_cache = FALSE
  )

  expect_identical(filtered$value, "filtered")
  expect_true(file.exists(cache_file))
  expect_identical(writes, 1L)

  all_records <- nbs_import(
    .file = source_file,
    .include.all = TRUE,
    .use_cache = TRUE
  )

  expect_identical(all_records$value, "all")
  expect_identical(writes, 1L)
  expect_identical(
    nanoparquet::read_parquet(cache_file)$value,
    "filtered"
  )
  expect_identical(include_all_values, c(FALSE, TRUE))
})

test_that("nbs_import returns an existing cache without rewriting it", {
  source_file <- file.path(withr::local_tempdir(), "NBS_EXPORT.CSV")
  utils::write.csv(data.frame(value = 1), source_file, row.names = FALSE)
  cache_file <- sub("\\.csv$", ".parquet", source_file, ignore.case = TRUE)
  nanoparquet::write_parquet(data.frame(value = "cached"), cache_file)

  writes <- 0L

  testthat::local_mocked_bindings(
    nbs_process = function(...) stop("nbs_process should not be called"),
    write_parquet = function(...) writes <<- writes + 1L,
    .package = "tarr"
  )

  result <- nbs_import(
    .file = source_file,
    .include.all = FALSE,
    .use_cache = TRUE
  )

  expect_identical(result$value, "cached")
  expect_identical(writes, 0L)
})
