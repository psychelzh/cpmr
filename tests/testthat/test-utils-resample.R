test_that("validate_resamples rejects malformed assessment sets", {
  expect_error(
    validate_resamples(list(), include_cases = 1:4),
    "non-empty list",
    fixed = FALSE
  )
  expect_error(
    validate_resamples(list(c(1, Inf), 2:3), include_cases = 1:4),
    "finite numeric indices",
    fixed = FALSE
  )
  expect_error(
    validate_resamples(list(1:2, 5:6), include_cases = 1:4),
    "contained in complete-case rows",
    fixed = FALSE
  )
})

test_that("crossv_kfold partitions inputs into non-overlapping folds", {
  withr::local_seed(123)
  folds <- crossv_kfold(1:6, 3)

  expect_length(folds, 3)
  expect_identical(sort(unlist(folds, use.names = FALSE)), 1:6)
})

test_that("resolve_resample_folds generates and validates public folds", {
  withr::local_seed(123)
  include_cases <- c(1L, 2L, 3L, 4L, 5L)

  resolved <- resolve_resample_folds(
    resamples = NULL,
    kfolds = 5,
    include_cases = include_cases
  )
  expect_identical(resolved$kfolds, 5L)
  expect_identical(
    sort(unlist(resolved$folds, use.names = FALSE)),
    include_cases
  )

  explicit <- resolve_resample_folds(
    resamples = list(1L, 2L, 3L, 4L, 5L),
    kfolds = NULL,
    include_cases = include_cases
  )
  expect_identical(explicit$kfolds, 5L)
  expect_identical(explicit$folds, list(1L, 2L, 3L, 4L, 5L))
})

test_that("warn_large_edge_storage signals large fold-wise storage", {
  expect_warning(
    warn_large_edge_storage(
      n_edges = 200000,
      kfolds = 10,
      return_edges = "all"
    ),
    "may consume large memory",
    fixed = FALSE
  )

  expect_invisible(warn_large_edge_storage(10, 5, "sum"))
})

test_that("compute_pooled_errors and correlations summarize predictions", {
  predictions <- data.frame(
    row = 1:4,
    fold = c(1L, 1L, 2L, 2L),
    real = c(1, 2, 3, 4),
    both = c(1, 2, 3, 4),
    pos = c(1, 2, 3, 4),
    neg = c(4, 3, 2, 1)
  )

  errors <- compute_pooled_errors(predictions)
  correlations <- compute_pooled_correlations(predictions)

  expect_identical(rownames(errors), c("rmse", "mae"))
  expect_named(correlations, c("both", "pos", "neg"))
  expect_equal(errors["rmse", "both"], 0)
  expect_equal(errors["mae", "both"], 0)
  expect_equal(unname(correlations["both"]), 1)
})

test_that("compute_pooled_metric_table returns a long metric table", {
  predictions <- data.frame(
    row = 1:4,
    fold = c(1L, 1L, 2L, 2L),
    real = c(1, 2, 3, 4),
    both = c(1, 2, 3, 4),
    pos = c(1, 2, 3, 4),
    neg = c(4, 3, 2, 1)
  )

  metrics <- compute_pooled_metric_table(
    predictions,
    metrics = c("rmse", "correlation")
  )

  expect_named(metrics, c("metric", "prediction", "estimate"))
  expect_equal(nrow(metrics), 6)
  expect_true(all(c("rmse", "correlation") %in% metrics$metric))
  expect_true(all(c("both", "pos", "neg") %in% metrics$prediction))
})

test_that("compute_fold_correlations summarizes each assessment fold", {
  predictions <- data.frame(
    row = 1:4,
    fold = c(1L, 1L, 2L, 2L),
    real = c(1, 2, 3, 4),
    both = c(1, 2, 3, 4),
    pos = c(1, 2, 3, 4),
    neg = c(4, 3, 2, 1)
  )
  folds <- list(1:2, 3:4)

  correlations <- compute_fold_correlations(predictions, folds)

  expect_named(correlations, c("fold", "n_assess", "both", "pos", "neg"))
  expect_equal(correlations$fold, 1:2)
  expect_equal(correlations$n_assess, c(2, 2))
  expect_true(all(is.finite(correlations$both[1:2])))
  expect_true(all(is.finite(correlations$pos[1:2])))
})

test_that("summarize_fold_correlations returns mean and standard error", {
  predictions <- data.frame(
    row = 1:6,
    fold = c(1L, 1L, 1L, 2L, 2L, 2L),
    real = c(1, 2, 3, 4, 5, 6),
    both = c(1, 2, 3, 6, 5, 4),
    pos = c(1, 2, 3, 4, 5, 6),
    neg = c(3, 2, 1, 6, 5, 4)
  )
  folds <- list(1:3, 4:6)

  summary <- summarize_fold_correlations(predictions, folds)

  expect_identical(rownames(summary), c("mean", "std_error"))
  expect_identical(colnames(summary), c("both", "pos", "neg"))
})

test_that("compute_fold_metric_table returns one row per fold, metric, and stream", {
  predictions <- data.frame(
    row = 1:6,
    fold = c(1L, 1L, 1L, 2L, 2L, 2L),
    real = c(1, 2, 3, 4, 5, 6),
    both = c(1, 2, 3, 6, 5, 4),
    pos = c(1, 2, 3, 4, 5, 6),
    neg = c(3, 2, 1, 6, 5, 4)
  )
  folds <- list(1:3, 4:6)

  metrics <- compute_fold_metric_table(
    predictions,
    folds,
    metrics = c("mae", "correlation")
  )

  expect_named(
    metrics,
    c("fold", "n_assess", "metric", "prediction", "estimate")
  )
  expect_equal(nrow(metrics), 12)
  expect_equal(sort(unique(metrics$fold)), 1:2)
  expect_equal(unique(metrics$n_assess), 3)
})

test_that("summarize_resample_edges handles sum, all, and none storage", {
  stored_sum <- matrix(
    c(2, 0, 1, 2),
    ncol = 2,
    dimnames = list(NULL, c("pos", "neg"))
  )
  stored_all <- array(
    c(
      TRUE,
      FALSE,
      FALSE,
      TRUE,
      FALSE,
      FALSE,
      TRUE,
      TRUE
    ),
    dim = c(2, 2, 2),
    dimnames = list(NULL, c("pos", "neg"), NULL)
  )

  expect_equal(summarize_resample_edges(stored_sum, "sum", 2L), stored_sum / 2)
  expect_equal(
    summarize_resample_edges(stored_all, "all", 2L),
    apply(stored_all, c(1, 2), mean)
  )
  expect_null(summarize_resample_edges(NULL, "none", 2L))
})
