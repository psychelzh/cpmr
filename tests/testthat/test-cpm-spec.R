test_that("spec stores staged model parameters", {
  s <- spec(
    selection = cpm_selection_cor(
      method = "spearman",
      criterion = "proportion",
      level = 0.05
    ),
    construction = cpm_construction_summary(
      sign_mode = "net",
      weight_scale = 0.02,
      standardize_edges = FALSE
    ),
    model = cpm_model_lm()
  )

  expect_s3_class(s, "cpm_spec")
  expect_s3_class(s$selection, "cpm_selection_spec")
  expect_s3_class(s$construction, "cpm_construction_spec")
  expect_s3_class(s$model, "cpm_model_spec")
  expect_identical(s$selection$type, "cor")
  expect_identical(s$selection$method, "spearman")
  expect_identical(s$selection$criterion, "proportion")
  expect_identical(s$selection$level, 0.05)
  expect_identical(s$construction$type, "summary")
  expect_identical(s$construction$sign_mode, "net")
  expect_identical(s$construction$weight_scale, 0.02)
  expect_false(s$construction$standardize_edges)
  expect_identical(s$model$type, "lm")
})

test_that("spec defaults to classic CPM edge handling", {
  spec <- spec()

  expect_identical(spec$selection$type, "cor")
  expect_s3_class(spec$selection, "cpm_selection_spec")
  expect_identical(spec$selection$method, "pearson")
  expect_identical(spec$selection$criterion, "p_value")
  expect_identical(spec$selection$level, 0.01)
  expect_identical(spec$construction$type, "summary")
  expect_s3_class(spec$construction, "cpm_construction_spec")
  expect_identical(spec$construction$sign_mode, "separate")
  expect_identical(spec$construction$weight_scale, 0)
  expect_false(spec$construction$standardize_edges)
  expect_s3_class(spec$model, "cpm_model_spec")
})

test_that("helper constructors validate scalar parameter values", {
  expect_error(
    cpm_selection_cor(level = -0.1),
    "`level` must be between 0 and 1.",
    fixed = TRUE
  )
  expect_error(
    cpm_selection_cor(level = c(0.1, 0.2)),
    "`level` must be a single finite number.",
    fixed = TRUE
  )
  expect_error(
    cpm_selection_cor(criterion = "absolute", level = -0.1),
    "`level` must be between 0 and 1.",
    fixed = TRUE
  )
  expect_error(
    cpm_selection_cor(criterion = "proportion", level = 1.1),
    "`level` must be between 0 and 1.",
    fixed = TRUE
  )
  expect_warning(
    cpm_selection_cor(level = 0),
    "`level` is at a boundary value (0 or 1); selection may become degenerate.",
    fixed = TRUE
  )
  expect_warning(
    cpm_selection_cor(level = 1),
    "`level` is at a boundary value (0 or 1); selection may become degenerate.",
    fixed = TRUE
  )
  expect_warning(
    cpm_selection_cor(criterion = "absolute", level = 0),
    "`level` is at a boundary value (0 or 1); selection may become degenerate.",
    fixed = TRUE
  )
  expect_warning(
    cpm_selection_cor(criterion = "absolute", level = 1),
    "`level` is at a boundary value (0 or 1); selection may become degenerate.",
    fixed = TRUE
  )
  expect_warning(
    cpm_selection_cor(criterion = "proportion", level = 0),
    "`level` is at a boundary value (0 or 1); selection may become degenerate.",
    fixed = TRUE
  )
  expect_warning(
    cpm_selection_cor(criterion = "proportion", level = 1),
    "`level` is at a boundary value (0 or 1); selection may become degenerate.",
    fixed = TRUE
  )
  expect_error(
    cpm_construction_summary(standardize_edges = NA),
    "`standardize_edges` must be either TRUE or FALSE.",
    fixed = TRUE
  )
  expect_error(
    cpm_construction_summary(weight_scale = -0.1),
    "`weight_scale` must be a single non-negative number.",
    fixed = TRUE
  )
  expect_error(
    cpm_construction_summary(standardize_edges = c(TRUE, FALSE)),
    "`standardize_edges` must be either TRUE or FALSE.",
    fixed = TRUE
  )
  expect_error(
    spec(selection = list()),
    "`selection` must be a `cpm_selection_spec` object.",
    fixed = TRUE
  )
  expect_error(
    spec(construction = list()),
    "`construction` must be a `cpm_construction_spec` object.",
    fixed = TRUE
  )
  expect_error(
    spec(model = list()),
    "`model` must be a `cpm_model_spec` object.",
    fixed = TRUE
  )
})

test_that("spec checks stage classes without unpacking stage internals", {
  custom_selection <- structure(
    list(type = "custom_selection"),
    class = "cpm_selection_spec"
  )
  custom_construction <- structure(
    list(type = "custom_construction"),
    class = "cpm_construction_spec"
  )
  custom_model <- structure(
    list(type = "custom_model"),
    class = "cpm_model_spec"
  )

  spec <- spec(
    selection = custom_selection,
    construction = custom_construction,
    model = custom_model
  )

  expect_identical(spec$selection, custom_selection)
  expect_identical(spec$construction, custom_construction)
  expect_identical(spec$model, custom_model)
})

test_that("helper constructors round-trip through params", {
  expect_s3_class(cpm_model_lm(), "cpm_model_spec")
  expect_identical(cpm_model_lm()$type, "lm")
})

test_that("print.cpm_spec shows readable staged settings", {
  spec <- spec(
    selection = cpm_selection_cor(
      method = "spearman",
      criterion = "absolute",
      level = 0.1
    ),
    construction = cpm_construction_summary(
      sign_mode = "net",
      weight_scale = 0.03,
      standardize_edges = TRUE
    )
  )

  expect_output(print(spec), "CPM specification")
  expect_output(print(spec), "Selection")
  expect_output(print(spec), "Method:\\s+spearman")
  expect_output(print(spec), "Criterion:\\s+absolute")
  expect_output(print(spec), "Construction")
  expect_output(print(spec), "Sign mode:\\s+net")
  expect_output(print(spec), "Outcome model:\\s+linear regression")
})

test_that("net construction yields a single prediction stream", {
  withr::local_seed(101)
  conmat <- matrix(rnorm(120), ncol = 12)
  behav <- rnorm(10)
  s <- spec(
    construction = cpm_construction_summary(sign_mode = "net")
  )

  result <- cpm(conmat = conmat, behav = behav, spec = s, resamples = 5)

  expect_named(result$predictions, c("row", "fold", "observed", "net"))
})

test_that("sigmoid edge weighting stores smooth edge weights in split models", {
  withr::local_seed(202)
  conmat <- matrix(rnorm(120), ncol = 12)
  behav <- rnorm(10)
  s <- spec(
    selection = cpm_selection_cor(
      criterion = "absolute",
      level = 0.1
    ),
    construction = cpm_construction_summary(
      weight_scale = 0.03
    )
  )

  edge_selection <- run_edge_selection(
    conmat = conmat,
    behav = behav,
    selection_spec = s$selection
  )
  model <- fit_split_model(
    conmat = conmat,
    behav = behav,
    edge_selection = edge_selection,
    construction_spec = s$construction,
    model_spec = s$model
  )

  expect_true(is.double(model$edge_weights))
  expect_equal(dim(model$edge_weights), c(ncol(conmat), 2))
  expect_true(any(
    model$edge_weights > 0 & model$edge_weights < 1
  ))
})

test_that("cpm returns predictions and folds", {
  withr::local_seed(123)
  conmat <- matrix(rnorm(100), ncol = 10)
  behav <- rnorm(10)
  s <- spec()

  res <- cpm(conmat = conmat, behav = behav, spec = s, resamples = 5)

  expect_s3_class(res, "cpm")
  expect_identical(as.character(res$call[[1]]), "cpm")
  expect_s3_class(res$spec, "cpm_spec")
  expect_identical(res$spec, s)
  expect_identical(length(res$folds), 5L)
  expect_equal(nrow(res$predictions), 10)
  expect_named(
    res$predictions,
    c("row", "fold", "observed", "joint", "positive", "negative")
  )
  expect_equal(dim(res$edges), c(10, 2))
  expect_s3_class(summary(res), "cpm_summary")
})

test_that("cpm uses spec() by default", {
  withr::local_seed(123)
  conmat <- matrix(rnorm(100), ncol = 10)
  behav <- rnorm(10)

  res <- cpm(conmat = conmat, behav = behav, resamples = 5)

  expect_s3_class(res, "cpm")
  expect_s3_class(res$spec, "cpm_spec")
})

test_that("cpm validates `spec` input", {
  conmat <- matrix(rnorm(100), ncol = 10)
  behav <- rnorm(10)

  expect_error(
    cpm(conmat = conmat, behav = behav, spec = list(), resamples = 5),
    "`spec` must be a CPM specification created by `spec()`.",
    fixed = TRUE
  )
})

test_that("cpm accepts custom resample indices", {
  withr::local_seed(123)
  conmat <- matrix(rnorm(120), ncol = 12)
  behav <- rnorm(10)
  spec <- spec()

  resamples <- list(1:3, 4:6, 7:10)
  res <- cpm(
    conmat = conmat,
    behav = behav,
    spec = spec,
    na_action = "exclude",
    resamples = resamples
  )

  expect_identical(length(res$folds), length(resamples))
  expect_identical(res$folds, resamples)
})

test_that("cpm validates fold-count resamples", {
  conmat <- matrix(rnorm(100), ncol = 10)
  behav <- rnorm(10)
  spec <- spec()

  expect_error(
    cpm(conmat = conmat, behav = behav, spec = spec, resamples = 1),
    "`resamples` must be NULL, a single integer greater than or equal to 2, or a non-empty list of assessment indices.",
    fixed = TRUE
  )
  expect_error(
    cpm(conmat = conmat, behav = behav, spec = spec, resamples = 2.5),
    "`resamples` must be NULL, a single integer greater than or equal to 2, or a non-empty list of assessment indices.",
    fixed = TRUE
  )
  expect_error(
    cpm(conmat = conmat, behav = behav, spec = spec, resamples = 11),
    "`resamples` as a fold count must be less than or equal to complete-case observations.",
    fixed = TRUE
  )
})

test_that("cpm validates custom resamples", {
  withr::local_seed(123)
  conmat <- matrix(rnorm(100), ncol = 10)
  behav <- rnorm(10)
  spec <- spec()

  expect_error(
    cpm(
      conmat = conmat,
      behav = behav,
      spec = spec,
      resamples = list(1:6, 6:10)
    ),
    "must not overlap",
    fixed = FALSE
  )
  expect_error(
    cpm(
      conmat = conmat,
      behav = behav,
      spec = spec,
      resamples = list(1:5)
    ),
    "at least 2 assessment sets",
    fixed = FALSE
  )
  expect_error(
    cpm(
      conmat = conmat,
      behav = behav,
      spec = spec,
      resamples = list(c(1.5, 2.5), 3:10)
    ),
    "integer-valued indices",
    fixed = FALSE
  )
  expect_error(
    cpm(
      conmat = conmat,
      behav = behav,
      spec = spec,
      resamples = list(c(0, 1), 2:10)
    ),
    "positive indices",
    fixed = FALSE
  )
  expect_error(
    cpm(
      conmat = conmat,
      behav = behav,
      spec = spec,
      resamples = list(c(1, 1), 2:10)
    ),
    "must not contain duplicates",
    fixed = FALSE
  )
  expect_error(
    cpm(
      conmat = conmat,
      behav = behav,
      spec = spec,
      resamples = list(1:4, 5:8)
    ),
    "must cover all complete-case rows exactly once",
    fixed = TRUE
  )
})

test_that("cpm errors clearly for insufficient complete cases", {
  conmat <- matrix(rnorm(100), ncol = 10)
  behav <- rep(NA_real_, 10)
  spec <- spec()

  expect_error(
    cpm(conmat = conmat, behav = behav, spec = spec, na_action = "exclude"),
    "No complete-case observations available for CPM.",
    fixed = TRUE
  )

  behav[] <- NA_real_
  behav[1] <- 1
  expect_error(
    cpm(conmat = conmat, behav = behav, spec = spec, na_action = "exclude"),
    "CPM requires at least 2 complete-case observations to define assessment folds.",
    fixed = TRUE
  )
})

test_that("cpm errors when any fold leaves fewer than 3 training rows", {
  conmat <- matrix(rnorm(40), ncol = 10)
  behav <- rnorm(4)
  spec <- spec()

  expect_error(
    cpm(conmat = conmat, behav = behav, spec = spec, resamples = 2),
    "CPM fitting requires at least 3 training observations.",
    fixed = TRUE
  )

  expect_error(
    cpm(
      conmat = conmat,
      behav = behav,
      spec = spec,
      resamples = list(1L, 2:4)
    ),
    "CPM fitting requires at least 3 training observations.",
    fixed = TRUE
  )
})

test_that("cpm can store summed edges", {
  withr::local_seed(123)
  conmat <- matrix(rnorm(120), ncol = 12)
  behav <- rnorm(10)
  spec <- spec()

  res <- cpm(
    conmat = conmat,
    behav = behav,
    spec = spec,
    resamples = 5,
    return_edges = "sum"
  )

  edges <- res$edges
  expect_type(edges, "double")
  expect_equal(dim(edges), c(12, 2))
})

test_that("cpm can store fold-wise edges", {
  withr::local_seed(123)
  conmat <- matrix(rnorm(120), ncol = 12)
  behav <- rnorm(10)
  spec <- spec()

  res <- cpm(
    conmat = conmat,
    behav = behav,
    spec = spec,
    resamples = 5,
    return_edges = "all"
  )

  edges <- res$edges
  expect_type(edges, "logical")
  expect_equal(dim(edges), c(12, 2, 5))
})

test_that("cpm handles covariates in assessment pipeline", {
  withr::local_seed(42)
  n <- 24
  p <- 18
  cov <- rnorm(n)
  conmat <- matrix(rnorm(n * p) + rep(cov, p), nrow = n, ncol = p)
  behav <- cov * 1.5 + rnorm(n)
  covariates <- matrix(cov, ncol = 1)
  spec <- spec()

  res <- cpm(
    conmat = conmat,
    behav = behav,
    spec = spec,
    covariates = covariates,
    resamples = 6,
    return_edges = "sum"
  )

  pred <- res$predictions
  expect_true(isTRUE(all(stats::complete.cases(pred$joint))))
  expect_true(isTRUE(res$settings$covariates))
  expect_equal(length(res$folds), 6L)
})

test_that("cpm fold path matches the corresponding internal split model", {
  withr::local_seed(7)
  n <- 15
  p <- 12
  conmat <- matrix(rnorm(n * p), nrow = n, ncol = p)
  behav <- rnorm(n)
  covariates <- matrix(rnorm(n * 2), ncol = 2)
  spec <- spec(
    selection = cpm_selection_cor(
      criterion = "p_value",
      level = 0.1
    )
  )
  resamples <- list(1:5, 6:10, 11:15)
  rows_test <- resamples[[1]]
  rows_train <- setdiff(seq_len(n), rows_test)
  covariates_train <- covariates[rows_train, , drop = FALSE]
  training_conmat <- regress_covariates_by_train(
    resp_train = conmat[rows_train, , drop = FALSE],
    cov_train = covariates_train
  )$train
  training_behav <- drop(
    regress_covariates_by_train(
      resp_train = behav[rows_train],
      cov_train = covariates_train
    )$train
  )
  assessment_conmat <- regress_covariates_by_train(
    resp_train = conmat[rows_train, , drop = FALSE],
    resp_test = conmat[rows_test, , drop = FALSE],
    cov_train = covariates_train,
    cov_test = covariates[rows_test, , drop = FALSE]
  )$test
  assessment_behav <- drop(
    regress_covariates_by_train(
      resp_train = behav[rows_train],
      resp_test = behav[rows_test],
      cov_train = covariates_train,
      cov_test = covariates[rows_test, , drop = FALSE]
    )$test
  )
  fold_edges <- select_edge_mask(
    conmat = training_conmat,
    behav = training_behav,
    method = spec$selection$method,
    criterion = spec$selection$criterion,
    level = spec$selection$level
  )
  fold_model <- fit_split_model(
    conmat = training_conmat,
    behav = training_behav,
    edge_selection = run_edge_selection(
      conmat = training_conmat,
      behav = training_behav,
      selection_spec = spec$selection
    ),
    construction_spec = spec$construction,
    model_spec = spec$model
  )
  result <- cpm(
    conmat = conmat,
    behav = behav,
    spec = spec,
    covariates = covariates,
    resamples = resamples,
    return_edges = "sum",
    na_action = "fail"
  )
  fold_pred <- predict_split_model(fold_model, assessment_conmat)
  collected <- result$predictions

  expect_equal(fold_model$edges, fold_edges)
  expect_equal(
    as.matrix(collected[rows_test, c("joint", "positive", "negative")]),
    fold_pred,
    ignore_attr = TRUE
  )
  expect_equal(collected$observed[rows_test], assessment_behav)
})

test_that("cpm excludes incomplete rows consistently with covariates", {
  withr::local_seed(11)
  n <- 15
  p <- 8
  conmat <- matrix(rnorm(n * p), nrow = n, ncol = p)
  behav <- rnorm(n)
  covariates <- matrix(rnorm(n * 2), ncol = 2)
  spec <- spec()

  behav[2] <- NA_real_
  conmat[5, 3] <- NA_real_
  covariates[8, 1] <- NA_real_

  include_cases <- setdiff(seq_len(n), c(2L, 5L, 8L))
  resamples <- list(include_cases[1:4], include_cases[5:8], include_cases[9:12])
  result <- cpm(
    conmat = conmat,
    behav = behav,
    spec = spec,
    covariates = covariates,
    resamples = resamples,
    return_edges = "sum",
    na_action = "exclude"
  )
  collected <- result$predictions

  expect_identical(result$folds, resamples)
  expect_equal(sort(collected$row[!is.na(collected$fold)]), include_cases)
  expect_true(isTRUE(all(stats::complete.cases(collected$joint[
    include_cases
  ]))))
  expect_true(isTRUE(all(is.na(collected$joint[-include_cases]))))
  expect_true(isTRUE(all(is.na(collected$fold[-include_cases]))))
})
