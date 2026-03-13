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

test_that("print.cpm_spec shows auto folds when kfolds is NULL", {
  spec <- cpm_spec()

  expect_output(print(spec), "CPM model specification")
  expect_output(print(spec), "Threshold method")
})

test_that("print.cpm_spec shows selected model options", {
  spec <- cpm_spec(thresh_method = "sparsity")

  expect_output(print(spec), "Threshold method: sparsity")
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
    "must cover all complete-case rows exactly once",
    fixed = TRUE
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

test_that("fit_resamples warns for large fold-wise edge storage", {
  withr::local_seed(123)
  conmat <- matrix(rnorm(20 * 140000), nrow = 20)
  behav <- rnorm(20)
  spec <- cpm_spec()

  expect_warning(
    fit_resamples(
      spec,
      conmat = conmat,
      behav = behav,
      kfolds = 10,
      return_edges = "all"
    ),
    "may consume large memory",
    fixed = TRUE
  )
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
