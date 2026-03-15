test_that("core_fit_xy returns a cpm_fit object with stored edges", {
  problem <- simulate_cpm_problem(n = 40, p = 20, seed = 11)

  fit <- core_fit_xy(
    conmat = problem$x,
    behav = problem$y,
    thresh_method = "sparsity",
    thresh_level = 0.2,
    bias_correct = TRUE,
    network = "both"
  )

  expect_s3_class(fit, "cpm_fit")
  expect_identical(fit$network, "both")
  expect_equal(dim(fit$edges), c(20, 2))
  expect_named(fit$params, c("thresh_method", "thresh_level", "bias_correct"))
})

test_that("predict.cpm_fit returns numeric and raw predictions", {
  problem <- simulate_cpm_problem(n = 50, p = 25, seed = 12)
  train_rows <- 1:35
  test_rows <- 36:50

  fit <- core_fit_xy(
    conmat = problem$x[train_rows, , drop = FALSE],
    behav = problem$y[train_rows],
    thresh_method = "sparsity",
    thresh_level = 0.2,
    network = "pos"
  )

  pred_num <- predict(fit, problem$x[test_rows, , drop = FALSE])
  pred_raw <- predict(fit, problem$x[test_rows, , drop = FALSE], type = "raw")

  expect_type(pred_num, "double")
  expect_length(pred_num, length(test_rows))
  expect_s3_class(pred_raw, "tbl_df")
  expect_named(pred_raw, c("both", "pos", "neg"))
  expect_equal(pred_num, pred_raw$pos)
})

test_that("core preprocessing keeps covariate regression train-only", {
  problem <- simulate_cpm_problem(n = 24, p = 10, seed = 13)
  covariates <- matrix(rnorm(24 * 2), nrow = 24, ncol = 2)
  rows_train <- 1:18
  rows_test <- 19:24

  training <- core_prepare_training_data(
    conmat = problem$x,
    behav = problem$y,
    covariates = covariates,
    rows_train = rows_train
  )
  assessment <- core_prepare_assessment_data(
    conmat = problem$x,
    behav = problem$y,
    covariates = covariates,
    rows_train = rows_train,
    rows_test = rows_test,
    covariates_train = training$covariates
  )

  expect_equal(dim(training$conmat), c(length(rows_train), ncol(problem$x)))
  expect_length(training$behav, length(rows_train))
  expect_equal(dim(assessment$conmat), c(length(rows_test), ncol(problem$x)))
  expect_length(assessment$behav, length(rows_test))
})

test_that("core resample helpers resolve folds and return metrics", {
  problem <- simulate_cpm_problem(n = 36, p = 18, seed = 14)

  withr::local_seed(99)
  resolved <- core_resolve_resample_folds(
    resamples = NULL,
    kfolds = 6,
    include_cases = seq_len(nrow(problem$x))
  )

  expect_length(resolved$folds, 6)
  expect_identical(
    sort(unname(unlist(resolved$folds))),
    seq_len(nrow(problem$x))
  )

  res <- core_fit_resamples(
    conmat = problem$x,
    behav = problem$y,
    resamples = resolved$folds,
    thresh_method = "sparsity",
    thresh_level = 0.2,
    network = "both",
    return_edges = "sum"
  )

  expect_named(res, c("folds", "edges", "metrics", "predictions", "params"))
  expect_equal(nrow(res$metrics), 6)
  expect_equal(nrow(res$predictions), nrow(problem$x))
  expect_equal(dim(res$edges), c(ncol(problem$x), 2))
})

test_that("core validators fail on malformed inputs", {
  problem <- simulate_cpm_problem(n = 10, p = 6, seed = 15)

  expect_error(
    core_fit_xy(problem$x, c("a", "b")),
    "Outcome data must be a numeric vector.",
    fixed = TRUE
  )
  expect_error(
    core_prepare_prediction_matrix(data.frame(a = letters[1:3])),
    "`new_data` must contain only numeric predictors.",
    fixed = TRUE
  )
  expect_error(
    core_validate_thresh_level(2),
    "`thresh_level` must be a single number between 0 and 1.",
    fixed = TRUE
  )
  expect_error(
    core_validate_resamples(list(1:2, 2:3), include_cases = 1:3),
    "must not overlap",
    fixed = FALSE
  )
})
