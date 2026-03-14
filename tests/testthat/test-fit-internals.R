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
  core_model <- core_train_model_impl(
    conmat = training$conmat,
    behav = training$behav,
    edges = edges,
    bias_correct = TRUE
  )
  expect_equal(model, core_model)
  expect_equal(
    predict_cpm_model(model, assessment$conmat),
    core_predict_model_impl(core_model, assessment$conmat)
  )
})
