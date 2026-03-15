test_that("input helpers validate types, sizes, and names", {
  conmat <- matrix(1:9, nrow = 3)
  rownames(conmat) <- c("s1", "s2", "s3")
  behav <- c(s1 = 1, s2 = 2, s3 = 3)
  covariates <- matrix(1:6, nrow = 3)
  rownames(covariates) <- c("s1", "s2", "s3")

  normalized <- core_normalize_inputs(conmat, behav, covariates)
  expect_equal(dim(normalized$conmat), c(3, 3))
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
  expect_named(as.data.frame(pred), inc_edges)
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

test_that("print method and assessment preprocessing cover remaining branches", {
  problem <- simulate_cpm_problem(n = 24, p = 8, seed = 51)
  covariates <- matrix(rnorm(24 * 2), nrow = 24, ncol = 2)

  fit <- core_fit_xy(
    conmat = problem$x[1:18, , drop = FALSE],
    behav = problem$y[1:18],
    thresh_method = "alpha",
    thresh_level = 0.1,
    network = "both"
  )

  printed <- paste(capture.output(print(fit)), collapse = "\n")
  expect_match(printed, "CPM engine fit")
  expect_match(printed, "Threshold method: alpha")

  assessment <- core_prepare_assessment_data(
    conmat = problem$x,
    behav = problem$y,
    covariates = covariates,
    rows_train = 1:18,
    rows_test = 19:24
  )

  expect_equal(dim(assessment$conmat), c(6, 8))
  expect_length(assessment$behav, 6)
})

test_that("resample validation covers error paths and warnings", {
  include_cases <- 1:6

  expect_null(core_validate_kfolds(NULL))
  expect_equal(core_validate_kfolds(3), 3L)
  expect_error(
    core_validate_kfolds(1),
    "must be NULL or a single integer greater than or equal to 2",
    fixed = FALSE
  )
  expect_error(
    core_validate_resamples("bad", include_cases),
    "`resamples` must be a non-empty list of assessment indices.",
    fixed = TRUE
  )
  expect_error(
    core_validate_resamples(list(1:2), include_cases),
    "must contain at least 2 assessment sets",
    fixed = FALSE
  )
  expect_error(
    core_validate_resamples(list(c(1, NA), 3:6), include_cases),
    "finite numeric indices",
    fixed = FALSE
  )
  expect_error(
    core_validate_resamples(list(c(1, 2.5), 3:6), include_cases),
    "integer-valued indices",
    fixed = FALSE
  )
  expect_error(
    core_validate_resamples(list(c(0, 1), 2:6), include_cases),
    "positive indices",
    fixed = FALSE
  )
  expect_error(
    core_validate_resamples(list(c(1, 1), 2:6), include_cases),
    "must not contain duplicates",
    fixed = FALSE
  )
  expect_error(
    core_validate_resamples(list(1:3, 4:7), include_cases),
    "contained in complete-case rows",
    fixed = FALSE
  )
  expect_error(
    core_validate_resamples(list(1:3, 3:6), include_cases),
    "must not overlap across folds",
    fixed = FALSE
  )
  expect_error(
    core_validate_resamples(list(1:2, 3:4), include_cases),
    "must cover all complete-case rows exactly once",
    fixed = FALSE
  )

  expect_error(
    core_resolve_resample_folds(list(1:3, 4:6), 2, include_cases),
    "Specify either `resamples` or `kfolds`, not both.",
    fixed = TRUE
  )
  expect_error(
    core_resolve_resample_folds(NULL, 7, include_cases),
    "`kfolds` must be less than or equal to complete-case observations.",
    fixed = TRUE
  )
  expect_error(
    core_resolve_resample_folds(list(1:4, 5:6), NULL, include_cases),
    "leave at least 3 complete-case training observations",
    fixed = FALSE
  )
  expect_warning(
    core_warn_large_edge_storage(700000, 2, "all"),
    "large memory",
    fixed = FALSE
  )
})

test_that("core fitting helpers cover remaining edge cases", {
  expect_error(
    core_fit_xy(matrix(1:4, nrow = 2), c(1, 2)),
    "At least 3 complete observations are required to fit CPM.",
    fixed = TRUE
  )
  expect_error(
    core_fit_resamples(
      conmat = matrix(c(NA_real_, NA_real_, NA_real_, NA_real_), nrow = 2),
      behav = c(1, 2),
      na_action = "exclude"
    ),
    "No complete-case observations available for resampling.",
    fixed = TRUE
  )
  expect_error(
    core_fit_resamples(
      conmat = matrix(c(1, 2, NA_real_, NA_real_), nrow = 2, byrow = TRUE),
      behav = c(1, 2),
      na_action = "exclude"
    ),
    "At least 2 complete-case observations are required for resampling.",
    fixed = TRUE
  )

  problem <- simulate_cpm_problem(n = 18, p = 9, seed = 52)
  res <- core_fit_resamples(
    conmat = problem$x,
    behav = problem$y,
    kfolds = 3,
    thresh_method = "alpha",
    thresh_level = 0.1,
    return_edges = "all"
  )

  expect_equal(dim(res$edges), c(9, 2, 3))
  expect_equal(
    core_safe_cor(c(1), c(1)),
    NA_real_
  )
  expect_equal(
    core_safe_cor(c(1, 1, 1), c(1, 2, 3)),
    NA_real_
  )
})

test_that("edge selection helpers cover alpha, warnings, and invalid methods", {
  alpha_edges <- core_select_edges(
    conmat = matrix(rnorm(30), nrow = 10),
    behav = rnorm(10),
    method = "alpha",
    level = 0.2
  )

  expect_equal(dim(alpha_edges), c(3, 2))
  expect_gt(core_critical_r(10, 0.05), 0)
  expect_warning(
    core_select_edges(
      conmat = cbind(1:10, 2:11, 3:12),
      behav = 1:10,
      method = "sparsity",
      level = 0.5
    ),
    "Not enough positive or negative correlation values.",
    fixed = TRUE
  )
  expect_error(
    core_select_edges(
      conmat = matrix(rnorm(30), nrow = 10),
      behav = rnorm(10),
      method = "bogus",
      level = 0.2
    ),
    "Invalid threshold method.",
    fixed = TRUE
  )
})

test_that("metrics and edge collectors cover remaining methods", {
  metrics_data <- tibble::tibble(
    truth = c(1, 2, 3, 4),
    estimate = c(1, 2, 4, 5)
  )

  spearman_res <- cpm_spearman(metrics_data, truth, estimate)
  expect_named(spearman_res, c(".metric", ".estimator", ".estimate"))
  expect_equal(cpm_spearman_vec(1:4, c(2, 3, 4, 5)), 1)
  expect_equal(cpm_cor_vec(c(1, NA), c(1, 2), na_rm = FALSE), NA_real_)
  expect_equal(
    cpm_spearman_vec(c(1, NA), c(1, 2), na_rm = FALSE),
    NA_real_
  )

  fake_fit <- structure(list(fit = 1), class = "model_fit")
  expect_error(
    collect_edges(fake_fit),
    "only supports `model_fit` objects created by the cpmr engine",
    fixed = FALSE
  )
})
