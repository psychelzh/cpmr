test_that("validate_kfolds accepts NULL and scalar fold counts", {
  expect_null(validate_kfolds(NULL))
  expect_identical(validate_kfolds(5), 5L)
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

test_that("run_single_fit matches fit() outputs on single data", {
  withr::local_seed(123)
  conmat <- matrix(rnorm(120), ncol = 12)
  behav <- rnorm(10)
  spec <- cpm_spec(thresh_method = "alpha", thresh_level = 0.05)
  call <- quote(fit(object = spec, conmat = conmat, behav = behav))

  internal_result <- run_single_fit(
    object = spec,
    conmat = conmat,
    behav = behav,
    covariates = NULL,
    na_action = "fail",
    call = call
  )
  api_result <- fit(
    spec,
    conmat = conmat,
    behav = behav,
    na_action = "fail"
  )

  expect_equal(internal_result$pred, api_result$pred)
  expect_equal(internal_result$edges, api_result$edges)
  expect_false("folds" %in% names(internal_result))
  expect_false("folds" %in% names(api_result))
  expect_equal(internal_result$model$models, api_result$model$models)
})

test_that("run_resample_fit matches fit_resamples() outputs", {
  withr::local_seed(321)
  conmat <- matrix(rnorm(120), ncol = 12)
  behav <- rnorm(10)
  spec <- cpm_spec(thresh_method = "sparsity", thresh_level = 0.2)

  withr::local_seed(999)
  folds <- crossv_kfold(seq_along(behav), 5)

  withr::local_seed(999)
  internal_result <- run_resample_fit(
    object = spec,
    conmat = conmat,
    behav = behav,
    covariates = NULL,
    folds = folds,
    return_edges = "sum",
    na_action = "fail"
  )
  api_result <- fit_resamples(
    spec,
    conmat = conmat,
    behav = behav,
    resamples = folds,
    return_edges = "sum",
    na_action = "fail"
  )

  expect_identical(internal_result$folds, api_result$folds)
  expect_equal(internal_result$edges, api_result$edges)
  expect_equal(internal_result$metrics, api_result$metrics)
  expect_equal(internal_result$predictions, api_result$predictions)
})

test_that("run_single_fit errors clearly on insufficient complete cases", {
  conmat <- matrix(rnorm(100), ncol = 10)
  behav <- rep(NA_real_, 10)
  spec <- cpm_spec()
  call <- quote(fit(object = spec, conmat = conmat, behav = behav))

  expect_error(
    run_single_fit(
      object = spec,
      conmat = conmat,
      behav = behav,
      covariates = NULL,
      na_action = "exclude",
      call = call
    ),
    "No complete-case observations available for fitting.",
    fixed = TRUE
  )

  behav[1:2] <- rnorm(2)
  expect_error(
    run_single_fit(
      object = spec,
      conmat = conmat,
      behav = behav,
      covariates = NULL,
      na_action = "exclude",
      call = call
    ),
    "At least 3 complete-case observations are required for fitting.",
    fixed = TRUE
  )
})

test_that("run_resample_fit errors clearly on insufficient complete cases", {
  conmat <- matrix(rnorm(100), ncol = 10)
  behav <- rep(NA_real_, 10)
  spec <- cpm_spec()
  folds <- list(1:5, 6:10)

  expect_error(
    run_resample_fit(
      object = spec,
      conmat = conmat,
      behav = behav,
      covariates = NULL,
      folds = folds,
      return_edges = "sum",
      na_action = "exclude"
    ),
    "No complete-case observations available for resampling.",
    fixed = TRUE
  )

  behav[] <- NA_real_
  behav[1] <- 1
  expect_error(
    run_resample_fit(
      object = spec,
      conmat = conmat,
      behav = behav,
      covariates = NULL,
      folds = list(1L),
      return_edges = "sum",
      na_action = "exclude"
    ),
    "At least 2 complete-case observations are required for resampling.",
    fixed = TRUE
  )
})
