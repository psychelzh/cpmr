test_that("validate_manual_resamples rejects malformed assessment sets", {
  expect_error(
    validate_manual_resamples(list(), include_cases = 1:4),
    "non-empty list",
    fixed = FALSE
  )
  expect_error(
    validate_manual_resamples(list(c(1, Inf), 2:3), include_cases = 1:4),
    "finite numeric indices",
    fixed = FALSE
  )
  expect_error(
    validate_manual_resamples(list(1:2, 5:6), include_cases = 1:4),
    "contained in complete-case rows",
    fixed = FALSE
  )
})

test_that("assert_normalized_resample_folds only checks normalized fold contracts", {
  expect_invisible(assert_normalized_resample_folds(list(1:2, 3:4)))

  expect_error(
    assert_normalized_resample_folds(list(1:2)),
    "at least 2 assessment index vectors",
    fixed = FALSE
  )
  expect_error(
    assert_normalized_resample_folds(list(c(1L, 1L), 2:3)),
    "without duplicates",
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
    resamples = 5,
    include_cases = include_cases
  )
  expect_identical(resolved$n_folds, 5L)
  expect_identical(
    sort(unlist(resolved$folds, use.names = FALSE)),
    include_cases
  )

  explicit <- resolve_resample_folds(
    resamples = list(1L, 2L, 3L, 4L, 5L),
    include_cases = include_cases
  )
  expect_identical(explicit$n_folds, 5L)
  expect_identical(explicit$folds, list(1L, 2L, 3L, 4L, 5L))

  loo <- resolve_resample_folds(
    resamples = NULL,
    include_cases = include_cases
  )
  expect_identical(loo$n_folds, 5L)
  expect_identical(
    sort(unlist(loo$folds, use.names = FALSE)),
    include_cases
  )
})

test_that("validate_resample_count rejects malformed fold counts", {
  expect_error(
    validate_resample_count(1),
    "`resamples` must be NULL, a single integer greater than or equal to 2, or a non-empty list of assessment indices.",
    fixed = TRUE
  )
  expect_error(
    validate_resample_count(2.5),
    "`resamples` must be NULL, a single integer greater than or equal to 2, or a non-empty list of assessment indices.",
    fixed = TRUE
  )
  expect_identical(validate_resample_count(4), 4L)
})

test_that("warn_large_edge_storage signals large fold-wise storage", {
  expect_warning(
    warn_large_edge_storage(
      n_edges = 200000,
      n_folds = 10,
      return_edges = "all"
    ),
    "may consume large memory",
    fixed = FALSE
  )

  expect_invisible(warn_large_edge_storage(10, 5, "sum"))
})

test_that("compute_pooled_metric_table summarizes pooled predictions", {
  predictions <- data.frame(
    row = 1:4,
    fold = c(1L, 1L, 2L, 2L),
    observed = c(1, 2, 3, 4),
    joint = c(1, 2, 3, 4),
    positive = c(1, 2, 3, 4),
    negative = c(4, 3, 2, 1)
  )

  metrics <- compute_pooled_metric_table(
    predictions,
    metrics = c("rmse", "mae", "correlation")
  )

  expect_equal(
    metrics$estimate[
      metrics$metric == "rmse" & metrics$prediction == "joint"
    ],
    0
  )
  expect_equal(
    metrics$estimate[
      metrics$metric == "mae" & metrics$prediction == "joint"
    ],
    0
  )
  expect_equal(
    metrics$estimate[
      metrics$metric == "correlation" & metrics$prediction == "joint"
    ],
    1
  )
})

test_that("compute_pooled_metric_table returns a long metric table", {
  predictions <- data.frame(
    row = 1:4,
    fold = c(1L, 1L, 2L, 2L),
    observed = c(1, 2, 3, 4),
    joint = c(1, 2, 3, 4),
    positive = c(1, 2, 3, 4),
    negative = c(4, 3, 2, 1)
  )

  metrics <- compute_pooled_metric_table(
    predictions,
    metrics = c("rmse", "correlation")
  )

  expect_named(metrics, c("metric", "prediction", "estimate"))
  expect_equal(nrow(metrics), 6)
  expect_true(all(c("rmse", "correlation") %in% metrics$metric))
  expect_true(all(
    c("joint", "positive", "negative") %in% metrics$prediction
  ))
})

test_that("compute_resample_metric only validates correlation methods when needed", {
  expect_equal(
    compute_resample_metric(
      observed = c(1, 2),
      predicted = c(1, 2),
      metric = "rmse",
      correlation_method = "kendall"
    ),
    0
  )
})

test_that("compute_resample_summary_metrics combines pooled and foldwise summaries", {
  predictions <- data.frame(
    row = 1:6,
    fold = c(1L, 1L, 1L, 2L, 2L, 2L),
    observed = c(1, 2, 3, 4, 5, 6),
    joint = c(1, 2, 3, 6, 5, 4),
    positive = c(1, 2, 3, 4, 5, 6),
    negative = c(3, 2, 1, 6, 5, 4)
  )
  folds <- list(1:3, 4:6)

  metrics <- compute_resample_summary_metrics(predictions, folds)

  expect_named(
    metrics,
    c("level", "metric", "prediction", "estimate", "std_error", "method")
  )
  expect_true(all(c("pooled", "foldwise") %in% metrics$level))
  expect_true(all(c("rmse", "mae", "correlation") %in% metrics$metric))
  expect_equal(
    metrics$estimate[
      metrics$level == "pooled" &
        metrics$metric == "correlation" &
        metrics$prediction == "positive"
    ],
    1
  )
  expect_equal(
    metrics$std_error[
      metrics$level == "foldwise" &
        metrics$metric == "correlation" &
        metrics$prediction == "joint"
    ],
    safe_std_error(c(1, -1))
  )
})

test_that("compute_fold_metric_table returns one row per fold, metric, and stream", {
  predictions <- data.frame(
    row = 1:6,
    fold = c(1L, 1L, 1L, 2L, 2L, 2L),
    observed = c(1, 2, 3, 4, 5, 6),
    joint = c(1, 2, 3, 6, 5, 4),
    positive = c(1, 2, 3, 4, 5, 6),
    negative = c(3, 2, 1, 6, 5, 4)
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
    dimnames = list(NULL, c("positive", "negative"))
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
    dimnames = list(NULL, c("positive", "negative"), NULL)
  )

  expect_equal(summarize_resample_edges(stored_sum, "sum", 2L), stored_sum / 2)
  expect_equal(
    summarize_resample_edges(stored_all, "all", 2L),
    apply(stored_all, c(1, 2), mean)
  )
  expect_null(summarize_resample_edges(NULL, "none", 2L))
})
