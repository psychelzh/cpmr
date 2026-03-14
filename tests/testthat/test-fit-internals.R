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

test_that("compatibility wrappers delegate to core helpers", {
  withr::local_seed(1)
  n <- 12
  p <- 6
  conmat <- matrix(rnorm(n * p), nrow = n, ncol = p)
  behav <- rnorm(n)
  covariates <- matrix(rnorm(n * 2), nrow = n, ncol = 2)
  rows_train <- 1:8
  rows_test <- 9:12

  training <- prepare_training_data(conmat, behav, covariates, rows_train)
  core_training <- core_prepare_training_data(
    conmat = conmat,
    behav = behav,
    covariates = covariates,
    rows_train = rows_train
  )
  expect_equal(training, core_training)

  regressed <- regress_covariates_by_train(
    resp_train = conmat[rows_train, , drop = FALSE],
    resp_test = conmat[rows_test, , drop = FALSE],
    cov_train = covariates[rows_train, , drop = FALSE],
    cov_test = covariates[rows_test, , drop = FALSE]
  )
  core_regressed <- core_regress_covariates_by_train(
    resp_train = conmat[rows_train, , drop = FALSE],
    resp_test = conmat[rows_test, , drop = FALSE],
    cov_train = covariates[rows_train, , drop = FALSE],
    cov_test = covariates[rows_test, , drop = FALSE]
  )
  expect_equal(regressed, core_regressed)

  assessment <- prepare_assessment_data(
    conmat = conmat,
    behav = behav,
    covariates = covariates,
    rows_train = rows_train,
    rows_test = rows_test,
    covariates_train = training$covariates
  )
  core_assessment <- core_prepare_assessment_data(
    conmat = conmat,
    behav = behav,
    covariates = covariates,
    rows_train = rows_train,
    rows_test = rows_test,
    covariates_train = training$covariates
  )
  expect_equal(assessment, core_assessment)

  edges <- select_edges(training$conmat, training$behav, "alpha", 0.1)
  model <- train_cpm_model(
    conmat = training$conmat,
    behav = training$behav,
    edges = edges,
    bias_correct = TRUE
  )
  core_model <- core_train_model(
    conmat = training$conmat,
    behav = training$behav,
    edges = edges,
    bias_correct = TRUE
  )
  expect_equal(model, core_model)
  expect_equal(
    predict_cpm_model(model, assessment$conmat),
    core_predict_model(core_model, assessment$conmat)
  )
})

test_that("legacy fit and object constructors still delegate to core helpers", {
  withr::local_seed(99)
  conmat <- matrix(rnorm(120), nrow = 10, ncol = 12)
  behav <- rnorm(10)
  spec <- cpm_spec(thresh_method = "alpha", thresh_level = 0.05)
  call <- quote(fit(object = spec, conmat = conmat, behav = behav))

  fit_result <- fit_cpm_single(
    call = call,
    object = spec,
    conmat = conmat,
    behav = behav,
    covariates = NULL,
    return_edges = "sum",
    na_action = "fail"
  )
  core_fit_result <- core_fit_single(
    object = spec,
    conmat = conmat,
    behav = behav,
    covariates = NULL,
    return_edges = "sum",
    na_action = "fail",
    call = call
  )
  expect_equal(fit_result, core_fit_result)

  cpm_object <- new_cpm(
    call = call,
    folds = list(1:10),
    behav = behav,
    pred = fit_result$pred,
    edges = fit_result$edges,
    model = fit_result$model,
    spec = spec,
    params = fit_result$params
  )
  expect_s3_class(cpm_object, "cpm")
  expect_identical(cpm_object$call, call)

  metrics <- data.frame(
    fold = 1L,
    n_assess = 2L,
    both = 0.5,
    pos = 0.4,
    neg = 0.3
  )
  predictions <- data.frame(
    row = 1:2,
    fold = 1L,
    real = c(1, 2),
    both = c(1.1, 1.9),
    pos = c(1.0, 2.0),
    neg = c(0.9, 2.1)
  )
  resamples_object <- new_cpm_resamples(
    spec = spec,
    folds = list(1:2),
    edges = fit_result$edges,
    metrics = metrics,
    predictions = predictions,
    params = list(return_edges = "sum")
  )
  expect_s3_class(resamples_object, "cpm_resamples")
  expect_identical(resamples_object$metrics, metrics)
  expect_identical(resamples_object$predictions, predictions)
})
