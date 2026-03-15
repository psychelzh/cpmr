test_that("input helpers validate types, sizes, and names", {
  conmat <- matrix(1:9, nrow = 3)
  rownames(conmat) <- c("s1", "s2", "s3")
  behav <- c(s1 = 1, s2 = 2, s3 = 3)
  covariates <- matrix(1:6, nrow = 3)
  rownames(covariates) <- c("s1", "s2", "s3")

  normalized <- core_normalize_inputs(conmat, behav, covariates)
  expect_equal(dim(normalized$conmat), c(3, 3))
  expect_named(
    as.data.frame(normalized$conmat),
    c("edge_1", "edge_2", "edge_3")
  )
  expect_equal(unname(normalized$behav), unname(behav))
  expect_equal(dim(normalized$covariates), c(3, 2))

  expect_identical(core_validate_bias_correct(TRUE), TRUE)
  expect_error(
    core_validate_bias_correct(1),
    "`bias_correct` must be either TRUE or FALSE.",
    fixed = TRUE
  )
  expect_error(
    core_normalize_inputs(matrix("a", nrow = 2), c(1, 2)),
    "Predictor data must be numeric.",
    fixed = TRUE
  )
  expect_error(
    core_normalize_inputs(matrix(1:6, nrow = 3), c("a", "b", "c")),
    "Outcome data must be a numeric vector.",
    fixed = TRUE
  )
  expect_error(
    core_normalize_inputs(matrix(1:6, nrow = 3), c(1, 2)),
    "Case numbers of `conmat` and `behav` must match.",
    fixed = TRUE
  )
  expect_error(
    core_normalize_inputs(
      conmat,
      structure(c(1, 2, 3), names = c("x1", "x2", "x3"))
    ),
    "Case names of `conmat` must match those of outcome data.",
    fixed = TRUE
  )
  expect_error(
    core_normalize_inputs(conmat, behav, matrix("a", nrow = 3)),
    "Covariates must be numeric.",
    fixed = TRUE
  )
  expect_error(
    core_normalize_inputs(conmat, behav, matrix(1:4, nrow = 2)),
    "Case numbers of `covariates` and `behav` must match.",
    fixed = TRUE
  )
  bad_covariates <- matrix(1:6, nrow = 3)
  rownames(bad_covariates) <- c("x1", "x2", "x3")
  expect_error(
    core_normalize_inputs(conmat, behav, bad_covariates),
    "Case names of `covariates` must match those of outcome data.",
    fixed = TRUE
  )

  named_conmat <- conmat
  colnames(named_conmat) <- c("edge_1", "edge_1", "")
  repaired <- core_normalize_inputs(named_conmat, behav)
  expect_named(
    as.data.frame(repaired$conmat),
    c("edge_1", "edge_1_1", "edge_3")
  )
})

test_that("missing-value helpers resolve include cases and kfold defaults", {
  conmat <- matrix(c(1, 2, NA, 4, 5, 6), nrow = 3)
  behav <- c(1, 2, 3)
  covariates <- matrix(c(1, 2, 3, 4, 5, NA), nrow = 3)

  expect_equal(
    core_resolve_include_cases(
      matrix(1:9, nrow = 3),
      behav,
      NULL,
      na_action = "fail"
    ),
    1:3
  )
  expect_error(
    core_resolve_include_cases(conmat, behav, NULL, na_action = "fail"),
    "Missing values found in `conmat`",
    fixed = TRUE
  )
  expect_error(
    core_resolve_include_cases(
      matrix(1:9, nrow = 3),
      c(1, NA, 3),
      NULL,
      na_action = "fail"
    ),
    "Missing values found in `behav`",
    fixed = TRUE
  )
  expect_error(
    core_resolve_include_cases(
      matrix(1:9, nrow = 3),
      behav,
      matrix(c(1, 2, NA), ncol = 1),
      na_action = "fail"
    ),
    "Missing values found in `covariates`",
    fixed = TRUE
  )
  expect_equal(
    core_resolve_include_cases(
      conmat,
      behav,
      covariates,
      na_action = "exclude"
    ),
    1:2
  )
  expect_equal(core_resolve_kfolds(NULL, 1:4), 4L)
  expect_equal(core_resolve_kfolds(3L, 1:4), 3L)
})

test_that("edge and prediction initializers cover all storage modes", {
  conmat <- matrix(1:12, nrow = 3)

  all_edges <- core_init_edges("all", conmat, 4)
  sum_edges <- core_init_edges("sum", conmat, 4)
  none_edges <- core_init_edges("none", conmat, 4)
  pred <- core_init_pred(c(a = 1, b = 2, c = 3))

  expect_equal(dim(all_edges), c(4, 2, 4))
  expect_equal(dim(sum_edges), c(4, 2))
  expect_true(all(sum_edges == 0))
  expect_null(none_edges)
  expect_equal(dim(pred), c(3, 3))
  expect_named(as.data.frame(pred), prediction_networks)
})

test_that("prediction matrix preparation enforces numeric aligned predictors", {
  new_data <- data.frame(b = c(3, 4), a = c(1, 2))

  aligned <- core_prepare_prediction_matrix(new_data, c("a", "b"))
  expect_equal(colnames(aligned), NULL)
  expect_equal(aligned[, 1], c(1, 2))
  expect_equal(aligned[, 2], c(3, 4))

  expect_error(
    core_prepare_prediction_matrix(data.frame(a = letters[1:2])),
    "`new_data` must contain only numeric predictors.",
    fixed = TRUE
  )
  expect_error(
    core_prepare_prediction_matrix(
      data.frame(a = 1:2),
      predictor_names = c("a", "b")
    ),
    "must contain the same predictor columns used at fit time",
    fixed = TRUE
  )
  expect_error(
    core_prepare_prediction_matrix(
      unname(matrix(1:4, nrow = 2)),
      predictor_names = c("a", "b", "c")
    ),
    "must have the same number of predictor columns used at fit time",
    fixed = TRUE
  )
  expect_error(
    core_prepare_prediction_matrix(matrix("a", nrow = 2, ncol = 2)),
    "`new_data` must contain only numeric predictors.",
    fixed = TRUE
  )
})

test_that("input validators fail on malformed threshold inputs", {
  expect_error(
    core_validate_thresh_level(2),
    "`thresh_level` must be a single number between 0 and 1.",
    fixed = TRUE
  )
})
