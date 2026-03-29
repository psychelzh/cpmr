test_that("safe_cor returns NA for degenerate vectors and values for valid ones", {
  expect_true(is.na(safe_cor(c(1, 1, 1), c(1, 2, 3))))
  expect_true(is.na(safe_cor(c(1), c(1))))
  expect_equal(safe_cor(c(1, 2, 3), c(2, 4, 6)), 1)
})

test_that("summary correlation helpers produce stable summaries", {
  pooled <- data.frame(
    prediction = c("joint", "positive"),
    estimate = c(0.3, 0.2),
    method = "spearman",
    stringsAsFactors = FALSE
  )
  foldwise <- data.frame(
    fold = c(1L, 1L, 2L, 2L),
    n_assess = c(3L, 3L, 3L, 3L),
    prediction = c("joint", "positive", "joint", "positive"),
    estimate = c(0.2, 0.1, 0.4, 0.3),
    method = "spearman",
    stringsAsFactors = FALSE
  )

  metrics <- compute_summary_correlations(
    pooled_metrics = pooled,
    foldwise_metrics = foldwise,
    correlation_method = "spearman"
  )

  expect_named(
    metrics,
    c("level", "prediction", "estimate", "std_error", "method")
  )
  expect_equal(
    summary_correlation_values(
      metrics,
      level = "pooled",
      prediction_streams = c("joint", "positive", "negative")
    ),
    c(joint = 0.3, positive = 0.2, negative = NA_real_)
  )
  expect_equal(
    summary_correlation_values(
      metrics,
      level = "foldwise",
      prediction_streams = c("joint", "positive", "negative")
    ),
    c(joint = 0.3, positive = 0.2, negative = NA_real_)
  )
  expect_true(all(metrics$method == "spearman"))
})

test_that("safe summary helpers handle empty and missing values", {
  expect_true(is.na(safe_mean(numeric())))
  expect_true(is.na(safe_mean(c(NA_real_, NA_real_))))
  expect_equal(safe_mean(c(1, NA_real_, 3)), 2)

  expect_true(is.na(safe_std_error(c(1))))
  expect_true(is.na(safe_std_error(c(NA_real_, NA_real_))))
  expect_equal(safe_std_error(c(1, 3)), 1)
})
