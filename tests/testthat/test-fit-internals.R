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

test_that("fit_cpm_single validates logical return_edges", {
  withr::local_seed(1)
  conmat <- matrix(rnorm(200), ncol = 10)
  behav <- rnorm(20)

  expect_error(
    fit_cpm_single(
      call = quote(fit(cpm_spec(), conmat, behav)),
      object = cpm_spec(thresh_method = "sparsity", thresh_level = 0.2),
      conmat = conmat,
      behav = behav,
      covariates = NULL,
      return_edges = "sum",
      na_action = "fail"
    ),
    "`return_edges` must be either TRUE or FALSE in `fit_cpm_single()`.",
    fixed = TRUE
  )
})

test_that("prepare_assessment_data derives training covariates when omitted", {
  withr::local_seed(2)
  conmat <- matrix(rnorm(120), ncol = 6)
  behav <- rnorm(20)
  covariates <- matrix(rnorm(40), ncol = 2)
  rows_train <- c(1L, 3L, 5L, 7L, 9L, 11L)
  rows_test <- c(2L, 4L, 6L)

  expected <- prepare_assessment_data(
    conmat,
    behav,
    covariates,
    rows_train,
    rows_test,
    covariates_train = covariates[rows_train, , drop = FALSE]
  )
  derived <- prepare_assessment_data(
    conmat,
    behav,
    covariates,
    rows_train,
    rows_test
  )

  expect_equal(derived, expected)
})
