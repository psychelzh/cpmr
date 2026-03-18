prediction_streams <- c("joint", "positive", "negative")

example_resample_summary <- function(
  metrics = rbind(
    data.frame(
      level = "pooled",
      metric = rep(c("rmse", "mae", "correlation"), each = 3),
      prediction = rep(prediction_streams, times = 3),
      estimate = c(
        0.8,
        0.9,
        1.0,
        0.6,
        0.7,
        0.8,
        0.4,
        0.2,
        -0.1
      ),
      std_error = NA_real_,
      method = c(
        rep(NA_character_, 6),
        rep("pearson", 3)
      ),
      stringsAsFactors = FALSE
    ),
    data.frame(
      level = "foldwise",
      metric = "correlation",
      prediction = prediction_streams,
      estimate = c(0.35, 0.15, -0.05),
      std_error = c(0.05, 0.02, 0.01),
      method = "pearson",
      stringsAsFactors = FALSE
    )
  ),
  edges = NULL,
  params = list(
    kfolds = 5L,
    return_edges = "none",
    correlation_method = "pearson",
    prediction_streams = prediction_streams
  )
) {
  structure(
    list(
      metrics = metrics,
      edges = edges,
      params = params
    ),
    class = "cpm_resamples_summary"
  )
}

test_that("Works for basic summary", {
  withr::local_seed(123)
  conmat <- matrix(rnorm(10000), nrow = 10)
  behav <- rnorm(10)
  summary_result <- summary(fit(cpm_spec(), conmat, behav))
  expect_s3_class(summary_result, "cpm_summary")
  expect_snapshot(summary_result)
})

test_that("summary.cpm returns NA when fewer than two valid pairs", {
  sparse_object <- structure(
    list(
      predictions = data.frame(
        row = 1:3,
        real = c(NA_real_, 2, NA_real_),
        joint = c(1, 2, 3),
        positive = c(1, 2, 3),
        negative = c(1, 2, 3)
      ),
      edges = matrix(
        c(TRUE, FALSE, FALSE, TRUE),
        ncol = 2,
        dimnames = list(NULL, c("positive", "negative"))
      ),
      params = list()
    ),
    class = "cpm"
  )

  summary_result <- summary(sparse_object)

  expect_true(all(is.na(summary_metric_values(
    summary_result$metrics,
    level = "single",
    metric = "correlation"
  ))))
  expect_identical(summary_result$edges, sparse_object$edges)
})

test_that("print.cpm_summary reports NA edge rates when stored edges are all missing", {
  summary_result <- structure(
    list(
      metrics = data.frame(
        level = "single",
        metric = "correlation",
        prediction = prediction_streams,
        estimate = c(0.1, 0.2, 0.3),
        std_error = NA_real_,
        method = "pearson",
        stringsAsFactors = FALSE
      ),
      edges = matrix(
        c(NA, NA),
        ncol = 2,
        dimnames = list(NULL, c("positive", "negative"))
      ),
      params = list(
        method = "pearson",
        prediction_streams = prediction_streams
      )
    ),
    class = "cpm_summary"
  )

  output <- capture.output(print(summary_result))

  expect_true(any(grepl("Positive: NA", output, fixed = TRUE)))
  expect_true(any(grepl("Negative: NA", output, fixed = TRUE)))
})

test_that("summary.cpm_resamples reports pooled errors, correlations, and edge rates", {
  predictions <- data.frame(
    row = 1:6,
    fold = c(1L, 1L, 1L, 2L, 2L, 2L),
    real = c(1, 2, 3, 4, 5, 6),
    joint = c(1, 2, 3, 6, 5, 4),
    positive = c(1, 2, 3, 4, 5, 6),
    negative = c(3, 2, 1, 6, 5, 4)
  )
  folds <- list(1:3, 4:6)
  summary_result <- summary(
    new_cpm_resamples(
      call = quote(fit_resamples(spec, conmat = conmat, behav = behav)),
      spec = cpm_spec(),
      params = list(kfolds = 2L, return_edges = "sum"),
      predictions = predictions,
      edges = matrix(
        c(2, 0, 1, 2),
        ncol = 2,
        dimnames = list(NULL, c("positive", "negative"))
      ),
      folds = folds
    )
  )
  fold_correlations <- compute_fold_metric_table(
    predictions,
    folds,
    metrics = "correlation"
  )

  expect_s3_class(summary_result, "cpm_resamples_summary")
  expect_identical(
    names(summary_result$metrics),
    c("level", "metric", "prediction", "estimate", "std_error", "method")
  )
  expect_equal(
    summary_metric_matrix(
      summary_result$metrics,
      level = "pooled",
      metric = c("rmse", "mae")
    ),
    rbind(
      rmse = vapply(
        prediction_streams,
        function(type) safe_rmse(predictions$real, predictions[[type]]),
        numeric(1)
      ),
      mae = vapply(
        prediction_streams,
        function(type) safe_mae(predictions$real, predictions[[type]]),
        numeric(1)
      )
    )
  )
  expect_equal(
    summary_metric_values(
      summary_result$metrics,
      level = "pooled",
      metric = "correlation"
    ),
    vapply(
      prediction_streams,
      function(type) safe_cor(predictions$real, predictions[[type]]),
      numeric(1)
    )
  )
  expect_equal(
    summary_metric_values(
      summary_result$metrics,
      level = "foldwise",
      metric = "correlation"
    ),
    vapply(
      prediction_streams,
      function(type) {
        safe_mean(fold_correlations$estimate[
          fold_correlations$prediction == type
        ])
      },
      numeric(1)
    )
  )
  expect_equal(
    summary_metric_values(
      summary_result$metrics,
      level = "foldwise",
      metric = "correlation",
      field = "std_error"
    ),
    vapply(
      prediction_streams,
      function(type) {
        safe_std_error(
          fold_correlations$estimate[fold_correlations$prediction == type]
        )
      },
      numeric(1)
    )
  )
  expect_equal(summary_result$edges[, "positive"], c(1, 0))
  expect_equal(summary_result$edges[, "negative"], c(0.5, 1))
})

test_that("summary.cpm_resamples returns NULL edges when resamples did not store them", {
  summary_result <- summary(
    new_cpm_resamples(
      call = quote(fit_resamples(spec, conmat = conmat, behav = behav)),
      spec = cpm_spec(),
      params = list(kfolds = 2L, return_edges = "none"),
      predictions = data.frame(
        row = 1:4,
        fold = c(1L, 1L, 2L, 2L),
        real = c(NA_real_, NA_real_, NA_real_, NA_real_),
        joint = c(1, 2, 3, 4),
        positive = c(1, 2, 3, 4),
        negative = c(4, 3, 2, 1)
      ),
      edges = NULL,
      folds = list(1:2, 3:4)
    )
  )

  expect_null(summary_result$edges)
  expect_true(all(is.na(summary_metric_values(
    summary_result$metrics,
    level = "pooled",
    metric = "correlation"
  ))))
  expect_true(all(is.na(summary_metric_values(
    summary_result$metrics,
    level = "foldwise",
    metric = "correlation"
  ))))
})

test_that("summary.cpm_resamples keeps default LOO summaries usable", {
  withr::local_seed(123)
  conmat <- matrix(rnorm(50), ncol = 10)
  behav <- rnorm(5)

  result <- fit_resamples(cpm_spec(), conmat = conmat, behav = behav)
  summary_result <- summary(result)

  expect_true(all(is.finite(summary_metric_matrix(
    summary_result$metrics,
    level = "pooled",
    metric = c("rmse", "mae")
  ))))
  expect_true(all(is.na(summary_metric_values(
    summary_result$metrics,
    level = "foldwise",
    metric = "correlation"
  ))))

  output <- capture.output(print(summary_result))
  expect_true(any(grepl("Pooled correlations (Pearson)", output, fixed = TRUE)))
  expect_true(any(grepl(
    "unavailable because they were undefined for all prediction streams",
    output,
    fixed = TRUE
  )))
})

test_that("summary.cpm_resamples supports single-stream net summaries", {
  withr::local_seed(321)
  conmat <- matrix(rnorm(120), nrow = 20)
  behav <- rowMeans(conmat[, 1:5, drop = FALSE]) + rnorm(20, sd = 0.2)

  result <- fit_resamples(
    cpm_spec(feature_space = "net"),
    conmat = conmat,
    behav = behav,
    kfolds = 4
  )
  summary_result <- summary(result)
  output <- capture.output(print(summary_result))

  expect_identical(summary_result$params$prediction_streams, "net")
  expect_equal(
    dim(summary_metric_matrix(
      summary_result$metrics,
      level = "pooled",
      metric = c("rmse", "mae"),
      prediction_streams = "net"
    )),
    c(2L, 1L)
  )
  expect_true(any(grepl("Net strength:", output, fixed = TRUE)))
})

test_that("summary.cpm_resamples averages fold-wise edges when all edges are stored", {
  stored_edges <- array(
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

  summary_result <- summary(
    new_cpm_resamples(
      call = quote(fit_resamples(spec, conmat = conmat, behav = behav)),
      spec = cpm_spec(),
      params = list(kfolds = 2L, return_edges = "all"),
      predictions = data.frame(
        row = 1:4,
        fold = c(1L, 1L, 2L, 2L),
        real = c(1, 2, 3, 4),
        joint = c(1, 2, 3, 4),
        positive = c(1, 2, 3, 4),
        negative = c(4, 3, 2, 1)
      ),
      edges = stored_edges,
      folds = list(1:2, 3:4)
    )
  )

  expect_equal(summary_result$edges, apply(stored_edges, c(1, 2), mean))
})

test_that("print.cpm_resamples_summary reports fold count and rates", {
  summary_result <- example_resample_summary(
    edges = matrix(
      c(0.5, 0.25),
      ncol = 2,
      dimnames = list(NULL, c("positive", "negative"))
    ),
    params = list(
      kfolds = 5L,
      return_edges = "sum",
      correlation_method = "pearson",
      prediction_streams = prediction_streams
    )
  )

  output <- capture.output(print(summary_result))

  expect_true(any(grepl("Number of folds: 5", output, fixed = TRUE)))
  expect_true(any(grepl("RMSE", output, fixed = TRUE)))
  expect_true(any(grepl("Joint: 0.400", output, fixed = TRUE)))
  expect_true(any(grepl(
    "Pooled correlations (Pearson):",
    output,
    fixed = TRUE
  )))
  expect_true(any(grepl("Joint: 0.350 (SE 0.050)", output, fixed = TRUE)))
  expect_true(any(grepl(
    "Fold-wise correlations (Pearson):",
    output,
    fixed = TRUE
  )))
  expect_true(any(grepl("Positive: 50.00%", output, fixed = TRUE)))
  expect_true(any(grepl("Negative: 25.00%", output, fixed = TRUE)))
})

test_that("print.cpm_resamples_summary omits edge block when edges are not stored", {
  summary_result <- example_resample_summary(
    metrics = example_resample_summary()$metrics
  )

  output <- capture.output(print(summary_result))

  expect_false(any(grepl("Selected edges", output, fixed = TRUE)))
})

test_that("print.cpm_resamples_summary notes when fold-wise correlations are undefined", {
  summary_result <- example_resample_summary(
    metrics = within(example_resample_summary()$metrics, {
      estimate[level == "foldwise" & metric == "correlation"] <- NA_real_
      std_error[level == "foldwise" & metric == "correlation"] <- NA_real_
    })
  )

  output <- capture.output(print(summary_result))

  expect_true(any(grepl(
    "unavailable because they were undefined for all prediction streams",
    output
  )))
})

test_that("print.cpm_resamples_summary prints fold-wise block when some streams remain estimable", {
  summary_result <- example_resample_summary(
    metrics = within(example_resample_summary()$metrics, {
      estimate[
        level == "foldwise" & metric == "correlation" & prediction == "positive"
      ] <- NA_real_
      std_error[
        level == "foldwise" & metric == "correlation" & prediction == "positive"
      ] <- NA_real_
    })
  )

  output <- capture.output(print(summary_result))

  expect_true(any(grepl(
    "Fold-wise correlations (Pearson):",
    output,
    fixed = TRUE
  )))
  expect_true(any(grepl("Joint: 0.350 (SE 0.050)", output, fixed = TRUE)))
  expect_true(any(grepl("Positive: NA", output, fixed = TRUE)))
  expect_true(any(grepl("Negative: -0.050", output, fixed = TRUE)))
  expect_false(any(grepl("SE NA", output, fixed = TRUE)))
  expect_false(any(grepl(
    "unavailable because they were undefined for all prediction streams",
    output,
    fixed = TRUE
  )))
})
