test_that("internal helpers validate nullable kfolds and finite resamples", {
  expect_null(validate_kfolds(NULL))
  expect_identical(validate_kfolds(5), 5L)

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

test_that("large fold-wise edge storage warning is emitted", {
  expect_warning(
    maybe_warn_large_edge_storage(
      n_edges = 140000,
      kfolds = 10,
      return_edges = "all"
    ),
    "may consume large memory",
    fixed = TRUE
  )
})
