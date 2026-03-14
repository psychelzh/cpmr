test_that("core_fit_single matches fit() outputs on single data", {
  withr::local_seed(123)
  conmat <- matrix(rnorm(120), ncol = 12)
  behav <- rnorm(10)
  spec <- cpm_spec(thresh_method = "alpha", thresh_level = 0.05)
  call <- quote(fit(object = spec, conmat = conmat, behav = behav))

  core_result <- core_fit_single(
    object = spec,
    conmat = conmat,
    behav = behav,
    covariates = NULL,
    return_edges = "sum",
    na_action = "fail",
    call = call
  )
  api_result <- fit(
    spec,
    conmat = conmat,
    behav = behav,
    return_edges = "sum",
    na_action = "fail"
  )

  expect_equal(core_result$pred, api_result$pred)
  expect_equal(core_result$edges, api_result$edges)
  expect_equal(core_result$folds, api_result$folds)
  expect_equal(core_result$model$models, api_result$model$models)
})

test_that("core_fit_resamples matches fit_resamples() outputs", {
  withr::local_seed(321)
  conmat <- matrix(rnorm(120), ncol = 12)
  behav <- rnorm(10)
  spec <- cpm_spec(thresh_method = "sparsity", thresh_level = 0.2)

  withr::local_seed(999)
  folds <- core_crossv_kfold(seq_along(behav), 5)

  withr::local_seed(999)
  core_result <- core_fit_resamples(
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

  expect_identical(core_result$folds, api_result$folds)
  expect_equal(core_result$edges, api_result$edges)
  expect_equal(core_result$metrics, api_result$metrics)
  expect_equal(core_result$predictions, api_result$predictions)
})

test_that("core_resolve_resample_folds generates and validates public folds", {
  include_cases <- 1:10

  withr::local_seed(222)
  resolved <- core_resolve_resample_folds(
    resamples = NULL,
    kfolds = 5,
    include_cases = include_cases
  )

  expect_length(resolved$folds, 5)
  expect_identical(sort(unname(unlist(resolved$folds))), include_cases)
  expect_identical(resolved$kfolds, 5L)

  explicit <- core_resolve_resample_folds(
    resamples = list(1:2, 3:4, 5:6, 7:8, 9:10),
    kfolds = NULL,
    include_cases = include_cases
  )
  expect_identical(explicit$folds, list(1:2, 3:4, 5:6, 7:8, 9:10))
  expect_identical(explicit$kfolds, 5L)
})

test_that("core_prepare_* keeps covariate handling train-only", {
  withr::local_seed(456)
  n <- 20
  p <- 8
  conmat <- matrix(rnorm(n * p), nrow = n, ncol = p)
  behav <- rnorm(n)
  covariates <- matrix(rnorm(n * 2), nrow = n, ncol = 2)
  rows_train <- 1:15
  rows_test <- 16:20

  training <- core_prepare_training_data(
    conmat = conmat,
    behav = behav,
    covariates = covariates,
    rows_train = rows_train
  )
  expected_cov_train <- covariates[rows_train, , drop = FALSE]
  expected_train_conmat <- core_regress_covariates(
    conmat[rows_train, , drop = FALSE],
    expected_cov_train
  )
  expected_train_behav <- drop(core_regress_covariates(
    behav[rows_train],
    expected_cov_train
  ))

  expect_equal(training$conmat, expected_train_conmat)
  expect_equal(training$behav, expected_train_behav)
  expect_equal(training$covariates, expected_cov_train)

  assessment <- core_prepare_assessment_data(
    conmat = conmat,
    behav = behav,
    covariates = covariates,
    rows_train = rows_train,
    rows_test = rows_test,
    covariates_train = training$covariates
  )
  expected_assess_conmat <- core_regress_covariates_by_train(
    conmat[rows_train, , drop = FALSE],
    conmat[rows_test, , drop = FALSE],
    expected_cov_train,
    covariates[rows_test, , drop = FALSE]
  )$test
<<<<<<< HEAD
  expected_assess_behav <- drop(
    core_regress_covariates_by_train(
      behav[rows_train],
      behav[rows_test],
      expected_cov_train,
      covariates[rows_test, , drop = FALSE]
    )$test
  )

  expect_equal(assessment$conmat, expected_assess_conmat)
  expect_equal(assessment$behav, expected_assess_behav)
})

test_that("core_prepare_assessment_data infers training covariates when omitted", {
  withr::local_seed(654)
  conmat <- matrix(rnorm(60), nrow = 10, ncol = 6)
  behav <- rnorm(10)
  covariates <- matrix(rnorm(20), nrow = 10, ncol = 2)
  rows_train <- 1:7
  rows_test <- 8:10

  assessment <- core_prepare_assessment_data(
    conmat = conmat,
    behav = behav,
    covariates = covariates,
    rows_train = rows_train,
    rows_test = rows_test
  )

  assessment_explicit <- core_prepare_assessment_data(
    conmat = conmat,
    behav = behav,
    covariates = covariates,
    rows_train = rows_train,
    rows_test = rows_test,
    covariates_train = covariates[rows_train, , drop = FALSE]
  )

  expect_equal(assessment, assessment_explicit)
})

test_that("core_fit_single errors clearly on insufficient complete cases", {
  conmat <- matrix(rnorm(100), ncol = 10)
  behav <- rep(NA_real_, 10)
  spec <- cpm_spec()
  call <- quote(fit(object = spec, conmat = conmat, behav = behav))

  expect_error(
    core_fit_single(
      object = spec,
      conmat = conmat,
      behav = behav,
      covariates = NULL,
      return_edges = "sum",
      na_action = "exclude",
      call = call
    ),
    "No complete-case observations available for fitting.",
    fixed = TRUE
  )

  behav[1:2] <- rnorm(2)
  expect_error(
    core_fit_single(
      object = spec,
      conmat = conmat,
      behav = behav,
      covariates = NULL,
      return_edges = "sum",
      na_action = "exclude",
      call = call
    ),
    "At least 3 complete-case observations are required for fitting.",
    fixed = TRUE
  )
})

test_that("core_fit_resamples errors clearly on insufficient complete cases", {
  conmat <- matrix(rnorm(100), ncol = 10)
  behav <- rep(NA_real_, 10)
  spec <- cpm_spec()
  folds <- list(1:5, 6:10)

  expect_error(
    core_fit_resamples(
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
    core_fit_resamples(
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
