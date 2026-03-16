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
