test_that("validate_kfolds accepts NULL and scalar fold counts", {
  expect_null(validate_kfolds(NULL))
  expect_identical(validate_kfolds(5), 5L)
})

test_that("resolve_kfolds uses complete-case count when NULL", {
  expect_identical(resolve_kfolds(NULL, include_cases = c(2L, 4L, 7L)), 3L)
  expect_identical(resolve_kfolds(5L, include_cases = c(2L, 4L, 7L)), 5L)
})

test_that("validate_resamples rejects malformed assessment sets", {
  expect_error(
    validate_resamples(list(), include_cases = 1:4),
    "non-empty list of assessment indices",
    fixed = FALSE
  )
  expect_error(
    validate_resamples(list(c(1, Inf), 2:3), include_cases = 1:4),
    "must contain finite numeric indices",
    fixed = FALSE
  )
  expect_error(
    validate_resamples(list(1:2, 5:6), include_cases = 1:4),
    "must be contained in complete-case rows",
    fixed = FALSE
  )
})

test_that("crossv_kfold partitions inputs into non-overlapping folds", {
  withr::local_seed(42)
  folds <- crossv_kfold(1:6, 3)

  expect_length(folds, 3)
  expect_identical(sort(unname(unlist(folds))), 1:6)
})

test_that("resolve_resample_folds generates and validates public folds", {
  include_cases <- 1:10

  withr::local_seed(222)
  resolved <- resolve_resample_folds(
    resamples = NULL,
    kfolds = 5,
    include_cases = include_cases
  )

  expect_length(resolved$folds, 5)
  expect_identical(sort(unname(unlist(resolved$folds))), include_cases)
  expect_identical(resolved$kfolds, 5L)

  explicit <- resolve_resample_folds(
    resamples = list(1:2, 3:4, 5:6, 7:8, 9:10),
    kfolds = NULL,
    include_cases = include_cases
  )
  expect_identical(explicit$folds, list(1:2, 3:4, 5:6, 7:8, 9:10))
  expect_identical(explicit$kfolds, 5L)
})

test_that("warn_large_edge_storage signals large fold-wise storage", {
  expect_warning(
    warn_large_edge_storage(
      n_edges = 140000,
      kfolds = 10,
      return_edges = "all"
    ),
    "may consume large memory",
    fixed = TRUE
  )
})
