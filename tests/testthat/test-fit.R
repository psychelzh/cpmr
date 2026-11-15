test_that("fit_split requires at least three training observations", {
  expect_error(
    fit_split(
      conmat = matrix(1:8, nrow = 4),
      behav = 1:4,
      covariates = NULL,
      rows_train = 1:2,
      rows_test = 3:4,
      selection_spec = cpm_selection_cor(),
      construction_spec = cpm_construction_strength(),
      model_spec = cpm_model_lm()
    ),
    "CPM fitting requires at least 3 training observations.",
    fixed = TRUE
  )
})

test_that("fit_split retains only prediction data and the selected-edge mask", {
  withr::local_seed(123)
  conmat <- matrix(rnorm(80), nrow = 10)
  behav <- rnorm(10)
  result <- fit_split(
    conmat = conmat,
    behav = behav,
    covariates = NULL,
    rows_train = 1:8,
    rows_test = 9:10,
    selection_spec = cpm_selection_cor(),
    construction_spec = cpm_construction_strength(),
    model_spec = cpm_model_lm()
  )

  expect_named(result, c("edge_mask", "observed", "predictions"))
  expect_equal(dim(result$edge_mask), c(8, 2))
  expect_equal(result$observed, behav[9:10])
  expect_equal(dim(result$predictions), c(2, 3))
})

test_that("covariate residual predictions respect rank-deficient QR pivots", {
  cov_train <- cbind(1, 1:4)
  cov_test <- matrix(c(1, 5), nrow = 1)
  response <- 2 + 3 * (1:4)
  vector_result <- regress_covariates_by_train(
    response,
    cov_train,
    17,
    cov_test
  )
  matrix_result <- regress_covariates_by_train(
    cbind(response, 2 * response),
    cov_train,
    matrix(c(17, 34), nrow = 1),
    cov_test
  )
  expect_equal(vector_result$rank, 2L)
  expect_equal(drop(vector_result$test), 0, tolerance = 1e-12)
  expect_equal(drop(matrix_result$test), c(0, 0), tolerance = 1e-12)
})

test_that("fit_split_model and predict_split_model compose correctly", {
  withr::local_seed(1)
  n <- 12
  p <- 6
  conmat <- matrix(rnorm(n * p), nrow = n, ncol = p)
  behav <- rnorm(n)
  covariates <- matrix(rnorm(n * 2), nrow = n, ncol = 2)
  rows_train <- 1:8
  rows_test <- 9:12

  covariates_train <- covariates[rows_train, , drop = FALSE]
  conmat_resid <- regress_covariates_by_train(
    resp_train = conmat[rows_train, , drop = FALSE],
    resp_test = conmat[rows_test, , drop = FALSE],
    cov_train = covariates_train,
    cov_test = covariates[rows_test, , drop = FALSE]
  )
  behav_resid <- regress_covariates_by_train(
    resp_train = behav[rows_train],
    resp_test = behav[rows_test],
    cov_train = covariates_train,
    cov_test = covariates[rows_test, , drop = FALSE]
  )
  training_conmat <- conmat_resid$train
  training_behav <- drop(behav_resid$train)
  assessment_conmat <- conmat_resid$test

  edge_selection <- run_edge_selection(
    conmat = training_conmat,
    behav = training_behav,
    selection_spec = cpm_selection_cor(
      criterion = "p_value",
      level = 0.1
    )
  )
  model <- fit_split_model(
    conmat = training_conmat,
    behav = training_behav,
    edge_selection = edge_selection,
    construction_spec = spec(
      construction = cpm_construction_strength(
        sign_mode = "separate",
        weight_scale = 0,
        standardize_edges = TRUE
      )
    )$construction,
    model_spec = cpm_model_lm()
  )

  expect_equal(dim(edge_selection$mask), c(p, 2))
  expect_named(model$outcome_models, c("joint", "positive", "negative"))
  expect_equal(
    dim(predict_split_model(model, assessment_conmat)),
    c(length(rows_test), 3)
  )
})

test_that("net sign mode with lm model produces a single stream", {
  withr::local_seed(2)
  conmat <- matrix(rnorm(40), nrow = 8, ncol = 5)
  behav <- rnorm(8)
  edge_selection <- list(
    associations = c(0.2, -0.2, NA_real_, 0.3, 0.4),
    mask = matrix(
      c(TRUE, FALSE, FALSE, TRUE, TRUE, FALSE, FALSE, TRUE, FALSE, TRUE),
      ncol = 2,
      dimnames = list(NULL, c("positive", "negative"))
    ),
    thresholds = c(positive = 0.1, negative = 0.1)
  )

  model <- fit_split_model(
    conmat = conmat,
    behav = behav,
    edge_selection = edge_selection,
    construction_spec = spec(
      construction = cpm_construction_strength(
        sign_mode = "net",
        weight_scale = 0,
        standardize_edges = FALSE
      )
    )$construction,
    model_spec = cpm_model_lm()
  )
  pred <- predict_split_model(model, conmat)

  expect_named(model$outcome_models, "net")
  expect_identical(colnames(pred), "net")
  expect_equal(dim(pred), c(nrow(conmat), 1))
})

test_that("fit_split_model handles aliased empty-sign features", {
  conmat <- cbind(
    rep(0, 5),
    1:5
  )
  behav <- 1:5
  edge_selection <- list(
    associations = c(0.3, -0.3),
    mask = matrix(
      c(TRUE, FALSE, FALSE, TRUE),
      ncol = 2,
      dimnames = list(NULL, c("positive", "negative"))
    ),
    thresholds = c(positive = 0.1, negative = 0.1)
  )
  model <- fit_split_model(
    conmat = conmat,
    behav = behav,
    edge_selection = edge_selection,
    construction_spec = spec()$construction,
    model_spec = cpm_model_lm()
  )
  fitted <- model$outcome_models$joint
  pred <- predict_split_model(model, conmat)

  expect_true(is.na(fitted$coefficients[["positive_summary"]]))
  expect_identical(fitted$prediction_coefficients[["positive_summary"]], 0)
  expect_equal(drop(pred[, "joint"]), behav)
})

test_that("fit_outcome_model rejects unsupported model types", {
  expect_error(
    fit_outcome_model(
      features = matrix(1:2, ncol = 1),
      behav = 1:2,
      model_spec = structure(list(type = "bogus"), class = "cpm_model_spec")
    ),
    "`model` must be a supported CPM outcome model.",
    fixed = TRUE
  )
})

test_that("predict_outcome_model validates unsupported model types", {
  expect_error(
    predict_outcome_model(
      fitted_model = list(type = "bogus"),
      features = matrix(1:2, ncol = 1)
    ),
    "`model` must be a supported CPM outcome model.",
    fixed = TRUE
  )
})
