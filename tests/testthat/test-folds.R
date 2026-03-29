test_that("normalize_manual_folds rejects malformed assessment sets", {
  expect_error(
    normalize_manual_folds(list(), include_cases = 1:4),
    "non-empty list",
    fixed = FALSE
  )
  expect_error(
    normalize_manual_folds(list(c(1, Inf), 2:3), include_cases = 1:4),
    "finite numeric indices",
    fixed = FALSE
  )
  expect_error(
    normalize_manual_folds(list(1:2, 5:6), include_cases = 1:4),
    "contained in complete-case rows",
    fixed = FALSE
  )
})

test_that("make_kfold_assessment_folds partitions inputs into non-overlapping folds", {
  withr::local_seed(123)
  folds <- make_kfold_assessment_folds(1:6, 3)

  expect_length(folds, 3)
  expect_identical(sort(unlist(folds, use.names = FALSE)), 1:6)
})

test_that("resolve_assessment_folds generates and validates public folds", {
  withr::local_seed(123)
  include_cases <- c(1L, 2L, 3L, 4L, 5L)

  resolved <- resolve_assessment_folds(
    resamples = 5,
    include_cases = include_cases
  )
  expect_identical(sort(unlist(resolved, use.names = FALSE)), include_cases)

  explicit <- resolve_assessment_folds(
    resamples = list(1L, 2L, 3L, 4L, 5L),
    include_cases = include_cases
  )
  expect_identical(explicit, list(1L, 2L, 3L, 4L, 5L))

  loo <- resolve_assessment_folds(
    resamples = NULL,
    include_cases = include_cases
  )
  expect_identical(sort(unlist(loo, use.names = FALSE)), include_cases)
})

test_that("resolve_assessment_folds requires enough complete cases to define folds", {
  expect_error(
    resolve_assessment_folds(
      resamples = NULL,
      include_cases = 1L
    ),
    "CPM requires at least 2 complete-case observations to define assessment folds.",
    fixed = TRUE
  )
})

test_that("normalize_fold_count rejects malformed fold counts", {
  expect_error(
    normalize_fold_count(1),
    "`resamples` must be NULL, a single integer greater than or equal to 2, or a non-empty list of assessment indices.",
    fixed = TRUE
  )
  expect_error(
    normalize_fold_count(2.5),
    "`resamples` must be NULL, a single integer greater than or equal to 2, or a non-empty list of assessment indices.",
    fixed = TRUE
  )
  expect_identical(normalize_fold_count(4), 4L)
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

test_that("correlation tables summarize pooled and fold-wise predictions", {
  predictions <- data.frame(
    row = 1:6,
    fold = c(1L, 1L, 1L, 2L, 2L, 2L),
    observed = c(1, 2, 3, 4, 5, 6),
    joint = c(1, 2, 3, 6, 5, 4),
    positive = c(1, 2, 3, 4, 5, 6),
    negative = c(3, 2, 1, 6, 5, 4)
  )
  folds <- list(1:3, 4:6)

  pooled <- compute_pooled_correlation_table(predictions)
  foldwise <- compute_fold_correlation_table(predictions, folds)

  expect_named(pooled, c("prediction", "estimate", "method"))
  expect_equal(nrow(pooled), 3)
  expect_equal(
    pooled$estimate[pooled$prediction == "positive"],
    1
  )

  expect_named(
    foldwise,
    c("fold", "n_assess", "prediction", "estimate", "method")
  )
  expect_equal(nrow(foldwise), 6)
  expect_equal(sort(unique(foldwise$fold)), 1:2)
  expect_equal(unique(foldwise$n_assess), 3)
})

test_that("summary correlations combine pooled and fold-wise estimates", {
  pooled <- data.frame(
    prediction = c("joint", "positive", "negative"),
    estimate = c(0.4, 0.2, -0.1),
    method = "pearson",
    stringsAsFactors = FALSE
  )
  foldwise <- data.frame(
    fold = c(1L, 1L, 2L, 2L, 1L, 2L),
    n_assess = c(3L, 3L, 3L, 3L, 3L, 3L),
    prediction = c(
      "joint",
      "joint",
      "positive",
      "positive",
      "negative",
      "negative"
    ),
    estimate = c(0.2, 0.4, 0.1, 0.3, -0.2, 0),
    method = "pearson",
    stringsAsFactors = FALSE
  )

  metrics <- compute_summary_correlations(
    pooled_metrics = pooled,
    foldwise_metrics = foldwise,
    correlation_method = "pearson"
  )

  expect_named(
    metrics,
    c("level", "prediction", "estimate", "std_error", "method")
  )
  expect_true(all(c("pooled", "foldwise") %in% metrics$level))
  expect_equal(
    summary_correlation_values(metrics, level = "pooled"),
    c(joint = 0.4, positive = 0.2, negative = -0.1)
  )
  expect_equal(
    summary_correlation_values(metrics, level = "foldwise"),
    c(joint = 0.3, positive = 0.2, negative = -0.1)
  )
})
