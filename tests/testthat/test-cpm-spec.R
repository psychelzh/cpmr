test_that("cpm_spec stores model parameters", {
  spec <- cpm_spec(
    thresh_method = "sparsity",
    thresh_level = 0.05,
    bias_correct = FALSE
  )

  expect_s3_class(spec, "cpm_spec")
  expect_identical(spec$params$thresh_method, "sparsity")
  expect_identical(spec$params$thresh_level, 0.05)
  expect_false(spec$params$bias_correct)
})

test_that("new_cpm_spec builds cpm_spec objects", {
  params <- list(
    thresh_method = "alpha",
    thresh_level = 0.05,
    bias_correct = TRUE
  )

  spec <- new_cpm_spec(params = params)

  expect_s3_class(spec, "cpm_spec")
  expect_identical(spec$params, params)
})

test_that("fit.cpm_spec returns a cpm object with correct call", {
  conmat <- matrix(rnorm(100), ncol = 10)
  behav <- rnorm(10)
  spec <- cpm_spec()
  result <- fit(spec, conmat = conmat, behav = behav)
  expect_s3_class(result, "cpm")
  expect_identical(as.character(result$call[[1]]), "fit")
  expect_s3_class(result$spec, "cpm_spec")
  expect_identical(result$spec, spec)
})

test_that("cpm_spec validates scalar parameter values", {
  expect_error(
    cpm_spec(thresh_level = -0.1),
    "`thresh_level` must be a single number between 0 and 1.",
    fixed = TRUE
  )
  expect_error(
    cpm_spec(thresh_level = c(0.1, 0.2)),
    "`thresh_level` must be a single number between 0 and 1.",
    fixed = TRUE
  )
  expect_error(
    cpm_spec(bias_correct = NA),
    "`bias_correct` must be either TRUE or FALSE.",
    fixed = TRUE
  )
  expect_error(
    cpm_spec(bias_correct = c(TRUE, FALSE)),
    "`bias_correct` must be either TRUE or FALSE.",
    fixed = TRUE
  )
})

test_that("print.cpm_spec shows model options", {
  spec <- cpm_spec(thresh_method = "sparsity")

  expect_output(print(spec), "CPM model specification")
  expect_output(print(spec), "Threshold method: sparsity")
  expect_output(print(spec), "Bias correction")
})

test_that("fit_resamples returns fold metrics and predictions", {
  withr::local_seed(123)
  conmat <- matrix(rnorm(100), ncol = 10)
  behav <- rnorm(10)
  spec <- cpm_spec()

  res <- fit_resamples(spec, conmat = conmat, behav = behav, kfolds = 5)

  expect_s3_class(res, "cpm_resamples")
  expect_s3_class(res$spec, "cpm_spec")
  expect_identical(length(res$folds), 5L)
  expect_equal(nrow(collect_metrics(res)), 5)
  expect_equal(nrow(collect_predictions(res)), 10)
  expect_named(
    collect_metrics(res),
    c("fold", "n_assess", "both", "pos", "neg")
  )
  expect_named(
    collect_predictions(res),
    c("row", "fold", "real", "both", "pos", "neg")
  )
  expect_null(collect_edges(res))
  expect_null(collect_edges(res, format = "index"))
})

test_that("fit_resamples accepts custom resample indices", {
  withr::local_seed(123)
  conmat <- matrix(rnorm(120), ncol = 12)
  behav <- rnorm(10)
  spec <- cpm_spec()

  resamples <- list(1:3, 4:6, 7:10)
  res <- fit_resamples(
    spec,
    conmat = conmat,
    behav = behav,
    na_action = "exclude",
    resamples = resamples
  )

  expect_identical(length(res$folds), length(resamples))
  expect_identical(res$folds, resamples)
})

test_that("fit_resamples validates kfolds", {
  conmat <- matrix(rnorm(100), ncol = 10)
  behav <- rnorm(10)
  spec <- cpm_spec()

  expect_error(
    fit_resamples(spec, conmat = conmat, behav = behav, kfolds = 1),
    "`kfolds` must be NULL or a single integer greater than or equal to 2.",
    fixed = TRUE
  )
  expect_error(
    fit_resamples(spec, conmat = conmat, behav = behav, kfolds = 2.5),
    "`kfolds` must be NULL or a single integer greater than or equal to 2.",
    fixed = TRUE
  )
  expect_error(
    fit_resamples(spec, conmat = conmat, behav = behav, kfolds = 11),
    "`kfolds` must be less than or equal to complete-case observations.",
    fixed = TRUE
  )
  expect_error(
    fit_resamples(
      spec,
      conmat = conmat,
      behav = behav,
      kfolds = 5,
      resamples = list(1:5, 6:10)
    ),
    "Specify either `resamples` or `kfolds`, not both.",
    fixed = TRUE
  )
})

test_that("fit_resamples validates custom resamples", {
  withr::local_seed(123)
  conmat <- matrix(rnorm(100), ncol = 10)
  behav <- rnorm(10)
  spec <- cpm_spec()

  expect_error(
    fit_resamples(
      spec,
      conmat = conmat,
      behav = behav,
      resamples = list(1:6, 6:10)
    ),
    "must not overlap",
    fixed = FALSE
  )
  expect_error(
    fit_resamples(spec, conmat = conmat, behav = behav, resamples = list(1:5)),
    "at least 2 assessment sets",
    fixed = FALSE
  )
  expect_error(
    fit_resamples(
      spec,
      conmat = conmat,
      behav = behav,
      resamples = list(c(1.5, 2.5), 3:10)
    ),
    "integer-valued indices",
    fixed = FALSE
  )
  expect_error(
    fit_resamples(
      spec,
      conmat = conmat,
      behav = behav,
      resamples = list(c(0, 1), 2:10)
    ),
    "positive indices",
    fixed = FALSE
  )
  expect_error(
    fit_resamples(
      spec,
      conmat = conmat,
      behav = behav,
      resamples = list(c(1, 1), 2:10)
    ),
    "must not contain duplicates",
    fixed = FALSE
  )
  expect_error(
    fit_resamples(
      spec,
      conmat = conmat,
      behav = behav,
      resamples = list(1:4, 5:8)
    ),
    "must cover all complete-case rows exactly once",
    fixed = TRUE
  )
})

test_that("fit_resamples errors clearly for insufficient complete cases", {
  conmat <- matrix(rnorm(100), ncol = 10)
  behav <- rep(NA_real_, 10)
  spec <- cpm_spec()

  expect_error(
    fit_resamples(spec, conmat = conmat, behav = behav, na_action = "exclude"),
    "No complete-case observations available for resampling.",
    fixed = TRUE
  )

  behav[] <- NA_real_
  behav[1] <- 1
  expect_error(
    fit_resamples(spec, conmat = conmat, behav = behav, na_action = "exclude"),
    "At least 2 complete-case observations are required for resampling.",
    fixed = TRUE
  )
})

test_that("fit_resamples errors when any fold leaves fewer than 3 training rows", {
  conmat <- matrix(rnorm(40), ncol = 10)
  behav <- rnorm(4)
  spec <- cpm_spec()

  expect_error(
    fit_resamples(spec, conmat = conmat, behav = behav, kfolds = 2),
    "Each resample must leave at least 3 complete-case training observations.",
    fixed = TRUE
  )

  expect_error(
    fit_resamples(
      spec,
      conmat = conmat,
      behav = behav,
      resamples = list(1L, 2:4)
    ),
    "Each resample must leave at least 3 complete-case training observations.",
    fixed = TRUE
  )
})

test_that("fit_resamples can store summed edges", {
  withr::local_seed(123)
  conmat <- matrix(rnorm(120), ncol = 12)
  behav <- rnorm(10)
  spec <- cpm_spec()

  res <- fit_resamples(
    spec,
    conmat = conmat,
    behav = behav,
    kfolds = 5,
    return_edges = "sum"
  )

  edges <- collect_edges(res)
  expect_type(edges, "double")
  expect_equal(dim(edges), c(12, 2))

  index_edges <- collect_edges(res, format = "index")
  expect_named(index_edges, c("pos", "neg"))
  expect_type(index_edges$pos, "integer")
  expect_type(index_edges$neg, "integer")
})

test_that("fit_resamples can store fold-wise edges", {
  withr::local_seed(123)
  conmat <- matrix(rnorm(120), ncol = 12)
  behav <- rnorm(10)
  spec <- cpm_spec()

  res <- fit_resamples(
    spec,
    conmat = conmat,
    behav = behav,
    kfolds = 5,
    return_edges = "all"
  )

  edges <- collect_edges(res)
  expect_type(edges, "logical")
  expect_equal(dim(edges), c(12, 2, 5))

  index_edges <- collect_edges(res, format = "index")
  expect_length(index_edges, 5)
  expect_named(index_edges[[1]], c("fold", "pos", "neg"))
  expect_identical(index_edges[[1]]$fold, 1L)
})

test_that("fit_resamples handles covariates in assessment pipeline", {
  withr::local_seed(42)
  n <- 24
  p <- 18
  cov <- rnorm(n)
  conmat <- matrix(rnorm(n * p) + rep(cov, p), nrow = n, ncol = p)
  behav <- cov * 1.5 + rnorm(n)
  covariates <- matrix(cov, ncol = 1)
  spec <- cpm_spec()

  res <- fit_resamples(
    spec,
    conmat = conmat,
    behav = behav,
    covariates = covariates,
    kfolds = 6,
    return_edges = "sum"
  )

  pred <- collect_predictions(res)
  expect_true(isTRUE(all(stats::complete.cases(pred$both))))
  expect_true(isTRUE(res$params$covariates))
  expect_equal(length(res$folds), 6L)
})

test_that("fit_resamples fold path matches fit() on the same training subset", {
  withr::local_seed(7)
  n <- 15
  p <- 12
  conmat <- matrix(rnorm(n * p), nrow = n, ncol = p)
  behav <- rnorm(n)
  covariates <- matrix(rnorm(n * 2), ncol = 2)
  spec <- cpm_spec(thresh_method = "alpha", thresh_level = 0.1)
  resamples <- list(1:5, 6:10, 11:15)
  rows_test <- resamples[[1]]
  rows_train <- setdiff(seq_len(n), rows_test)

  single_fit <- fit(
    spec,
    conmat = conmat[rows_train, , drop = FALSE],
    behav = behav[rows_train],
    covariates = covariates[rows_train, , drop = FALSE],
    na_action = "fail"
  )
  training <- prepare_training_data(
    conmat = conmat,
    behav = behav,
    covariates = covariates,
    rows_train = rows_train
  )
  assessment <- prepare_assessment_data(
    conmat = conmat,
    behav = behav,
    covariates = covariates,
    rows_train = rows_train,
    rows_test = rows_test,
    covariates_train = training$covariates
  )
  fold_edges <- select_edges(
    conmat = training$conmat,
    behav = training$behav,
    method = spec$params$thresh_method,
    level = spec$params$thresh_level
  )
  fold_model <- train_model(
    conmat = training$conmat,
    behav = training$behav,
    edges = fold_edges,
    bias_correct = spec$params$bias_correct
  )
  resampled <- fit_resamples(
    spec,
    conmat = conmat,
    behav = behav,
    covariates = covariates,
    resamples = resamples,
    return_edges = "sum",
    na_action = "fail"
  )
  fold_pred <- predict_model(fold_model, assessment$conmat)
  collected <- collect_predictions(resampled)

  expect_equal(single_fit$edges, fold_edges)
  expect_equal(single_fit$model$edges, fold_model$edges)
  expect_equal(single_fit$model$models, fold_model$models)
  expect_equal(single_fit$model$center, fold_model$center)
  expect_equal(single_fit$model$scale, fold_model$scale)
  expect_equal(predict_model(single_fit$model, assessment$conmat), fold_pred)
  expect_equal(
    as.matrix(collected[rows_test, c("both", "pos", "neg")]),
    fold_pred,
    ignore_attr = TRUE
  )
  expect_equal(collected$real[rows_test], assessment$behav)
})

test_that("fit_resamples excludes incomplete rows consistently with covariates", {
  withr::local_seed(11)
  n <- 15
  p <- 8
  conmat <- matrix(rnorm(n * p), nrow = n, ncol = p)
  behav <- rnorm(n)
  covariates <- matrix(rnorm(n * 2), ncol = 2)
  spec <- cpm_spec()

  behav[2] <- NA_real_
  conmat[5, 3] <- NA_real_
  covariates[8, 1] <- NA_real_

  include_cases <- setdiff(seq_len(n), c(2L, 5L, 8L))
  resamples <- list(include_cases[1:4], include_cases[5:8], include_cases[9:12])
  resampled <- fit_resamples(
    spec,
    conmat = conmat,
    behav = behav,
    covariates = covariates,
    resamples = resamples,
    return_edges = "sum",
    na_action = "exclude"
  )
  collected <- collect_predictions(resampled)

  expect_identical(resampled$folds, resamples)
  expect_equal(sort(collected$row[!is.na(collected$fold)]), include_cases)
  expect_true(isTRUE(all(stats::complete.cases(collected$both[include_cases]))))
  expect_true(isTRUE(all(is.na(collected$both[-include_cases]))))
  expect_true(isTRUE(all(is.na(collected$fold[-include_cases]))))
})
