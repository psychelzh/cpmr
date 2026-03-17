prediction_types <- c("combined", "positive", "negative")

test_that("safe_cor returns NA for degenerate vectors", {
  expect_true(is.na(safe_cor(c(1, 1, 1), c(1, 2, 3))))
  expect_true(is.na(safe_cor(c(1), c(1))))
})

test_that("safe_cor returns correlation for valid vectors", {
  expect_equal(safe_cor(c(1, 2, 3), c(2, 4, 6)), 1)
})

test_that("safe_rmse and safe_mae handle missing and valid pairs", {
  expect_true(is.na(safe_rmse(c(NA_real_), c(1))))
  expect_true(is.na(safe_mae(c(NA_real_), c(1))))
  expect_equal(safe_rmse(c(1, 2), c(1, 4)), sqrt(2))
  expect_equal(safe_mae(c(1, 2), c(1, 4)), 1)
})

test_that("safe_mean handles empty and all-NA inputs", {
  expect_true(is.na(safe_mean(numeric())))
  expect_true(is.na(safe_mean(c(NA_real_, NA_real_))))
  expect_equal(safe_mean(c(1, NA_real_, 3)), 2)
})

test_that("safe_std_error handles short and valid inputs", {
  expect_true(is.na(safe_std_error(c(1))))
  expect_true(is.na(safe_std_error(c(NA_real_, NA_real_))))
  expect_equal(safe_std_error(c(1, 3)), 1)
})

test_that("summary metric helpers produce stable long-format summaries", {
  metric_table <- data.frame(
    metric = c("rmse", "rmse", "correlation", "correlation"),
    prediction = c("combined", "positive", "combined", "positive"),
    estimate = c(0.1, 0.2, 0.3, 0.4),
    stringsAsFactors = FALSE
  )

  metrics <- as_summary_metrics(
    metric_table,
    level = "pooled",
    method = "pearson"
  )

  expect_named(
    metrics,
    c("level", "metric", "prediction", "estimate", "std_error", "method")
  )
  expect_true(all(is.na(metrics$std_error)))
  expect_identical(
    metrics$method,
    c(NA_character_, NA_character_, "pearson", "pearson")
  )
})

test_that("summary metric helpers summarize and extract metric views", {
  metric_table <- data.frame(
    fold = c(1L, 1L, 2L, 2L),
    n_assess = c(3L, 3L, 3L, 3L),
    metric = c("correlation", "correlation", "correlation", "correlation"),
    prediction = c("combined", "positive", "combined", "positive"),
    estimate = c(0.2, 0.1, 0.4, 0.3),
    stringsAsFactors = FALSE
  )

  metrics <- summarize_metric_estimates(
    metric_table,
    level = "foldwise",
    method = "spearman"
  )

  expect_equal(
    summary_metric_values(
      metrics,
      level = "foldwise",
      metric = "correlation",
      prediction_types = prediction_types
    ),
    c(combined = 0.3, positive = 0.2, negative = NA_real_)
  )
  expect_equal(
    summary_metric_method(metrics, level = "foldwise", metric = "correlation"),
    "spearman"
  )
  expect_true(is.na(summary_metric_method(
    metrics,
    level = "pooled",
    metric = "rmse"
  )))
  expect_equal(
    summary_metric_values(
      metrics,
      level = "pooled",
      metric = "rmse",
      prediction_types = prediction_types
    ),
    c(combined = NA_real_, positive = NA_real_, negative = NA_real_)
  )
  expect_equal(
    summary_metric_matrix(
      rbind(
        as_summary_metrics(
          data.frame(
            metric = c("rmse", "rmse", "mae", "mae"),
            prediction = c("combined", "positive", "combined", "positive"),
            estimate = c(1, 2, 3, 4),
            stringsAsFactors = FALSE
          ),
          level = "pooled"
        ),
        metrics
      ),
      level = "pooled",
      metric = c("rmse", "mae"),
      prediction_types = prediction_types
    ),
    rbind(
      rmse = c(combined = 1, positive = 2, negative = NA_real_),
      mae = c(combined = 3, positive = 4, negative = NA_real_)
    )
  )
})

test_that("summary_metric_matrix keeps matrix shape for a single prediction stream", {
  metrics <- as_summary_metrics(
    data.frame(
      metric = c("rmse", "mae"),
      prediction = c("difference", "difference"),
      estimate = c(1, 2),
      stringsAsFactors = FALSE
    ),
    level = "pooled"
  )

  expect_equal(
    summary_metric_matrix(
      metrics,
      level = "pooled",
      metric = c("rmse", "mae"),
      prediction_types = "difference"
    ),
    rbind(
      rmse = c(difference = 1),
      mae = c(difference = 2)
    )
  )
})
