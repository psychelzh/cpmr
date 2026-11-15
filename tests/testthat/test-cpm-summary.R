prediction_streams <- c("joint", "positive", "negative")

example_summary_result <- function() {
  withr::local_seed(123)
  conmat <- matrix(rnorm(10000), nrow = 20)
  behav <- rnorm(20)

  cpm(conmat = conmat, behav = behav, spec = spec(), resamples = 5)
}

example_cpm_summary <- function(
  metrics = rbind(
    data.frame(
      level = "pooled",
      metric = "correlation",
      prediction = prediction_streams,
      estimate = c(0.4, 0.2, -0.1),
      std_error = NA_real_,
      method = "pearson",
      n = 20L,
      n_folds = NA_integer_,
      stringsAsFactors = FALSE
    ),
    data.frame(
      level = "pooled",
      metric = "rmse",
      prediction = prediction_streams,
      estimate = c(0.7, 0.8, 0.9),
      std_error = NA_real_,
      method = NA_character_,
      n = 20L,
      n_folds = NA_integer_,
      stringsAsFactors = FALSE
    ),
    data.frame(
      level = "pooled",
      metric = "mae",
      prediction = prediction_streams,
      estimate = c(0.5, 0.6, 0.7),
      std_error = NA_real_,
      method = NA_character_,
      n = 20L,
      n_folds = NA_integer_,
      stringsAsFactors = FALSE
    ),
    data.frame(
      level = "foldwise",
      metric = "correlation",
      prediction = prediction_streams,
      estimate = c(0.35, 0.15, -0.05),
      std_error = c(0.05, 0.02, 0.01),
      method = "pearson",
      n = NA_integer_,
      n_folds = 5L,
      stringsAsFactors = FALSE
    )
  ),
  tables = list(
    pooled = data.frame(
      metric = rep(c("correlation", "rmse", "mae"), each = 3),
      prediction = rep(prediction_streams, 3),
      estimate = c(0.4, 0.2, -0.1, 0.7, 0.8, 0.9, 0.5, 0.6, 0.7),
      n = 20L,
      method = c(rep("pearson", 3), rep(NA_character_, 6)),
      stringsAsFactors = FALSE
    ),
    foldwise = data.frame(
      fold = rep(1:5, each = 3),
      metric = "correlation",
      n_assess = rep(4L, 15),
      prediction = rep(prediction_streams, times = 5),
      estimate = rep(c(0.35, 0.15, -0.05), 5),
      method = "pearson",
      stringsAsFactors = FALSE
    )
  ),
  edges = NULL,
  settings = list(
    n_folds = 5L,
    small_assessment_folds = FALSE,
    edge_storage = "not stored",
    correlation_method = "pearson",
    prediction_streams = prediction_streams
  )
) {
  if (!"descriptive_only" %in% names(metrics)) {
    metrics$descriptive_only <- FALSE
  }
  if (!"descriptive_only" %in% names(tables$pooled)) {
    tables$pooled$descriptive_only <- FALSE
  }

  structure(
    list(
      metrics = metrics,
      tables = tables,
      edges = edges,
      settings = settings
    ),
    class = "cpm_summary"
  )
}

test_that("format helpers produce stable display strings", {
  expect_identical(format_cor(0.1234), "0.123")
  expect_identical(format_cor(NA_real_), "NA")
  expect_identical(format_rate(0.125), "12.50%")
  expect_identical(format_rate(NA_real_), "NA")
  expect_identical(format_threshold_level(0.005), "0.005")
  expect_identical(format_threshold_level(0.2), "0.2")
})

test_that("prediction labels use full names", {
  expect_identical(prediction_label("joint"), "Joint")
  expect_identical(prediction_label("positive"), "Positive")
  expect_identical(prediction_label("negative"), "Negative")
  expect_identical(prediction_label("net"), "Net")
})

test_that("safe_cor requires three valid pairs and handles degenerate vectors", {
  expect_true(is.na(safe_cor(c(1, 1, 1), c(1, 2, 3))))
  expect_true(is.na(safe_cor(c(1), c(1))))
  expect_true(is.na(safe_cor(c(1, 2), c(2, 4))))
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
  pooled$metric <- "correlation"
  pooled$n <- 10L
  pooled$descriptive_only <- TRUE
  foldwise$metric <- "correlation"

  metrics <- compute_summary_metrics(
    pooled_metrics = pooled,
    foldwise_metrics = foldwise,
    correlation_method = "spearman"
  )

  expect_named(
    metrics,
    c(
      "level",
      "metric",
      "prediction",
      "estimate",
      "std_error",
      "method",
      "n",
      "n_folds",
      "descriptive_only"
    )
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
  expect_true(all(
    metrics$method[metrics$metric == "correlation"] == "spearman"
  ))
})

test_that("safe summary helpers handle empty and missing values", {
  expect_true(is.na(safe_mean(numeric())))
  expect_true(is.na(safe_mean(c(NA_real_, NA_real_))))
  expect_equal(safe_mean(c(1, NA_real_, 3)), 2)

  expect_true(is.na(safe_std_error(c(1))))
  expect_true(is.na(safe_std_error(c(NA_real_, NA_real_))))
  expect_equal(safe_std_error(c(1, 3)), 1)
})

test_that("pooled metric tables compute errors from complete prediction pairs", {
  predictions <- data.frame(
    row = 1:4,
    fold = 1:4,
    observed = c(1, 2, 3, 4),
    joint = c(1, 4, NA_real_, 2),
    positive = c(2, 2, 2, 2),
    negative = c(NA_real_, 1, 3, 4)
  )

  metrics <- compute_pooled_metric_table(predictions)
  joint_rmse <- metrics$estimate[
    metrics$metric == "rmse" & metrics$prediction == "joint"
  ]
  joint_mae <- metrics$estimate[
    metrics$metric == "mae" & metrics$prediction == "joint"
  ]
  joint_n <- metrics$n[
    metrics$metric == "rmse" & metrics$prediction == "joint"
  ]

  expect_equal(joint_rmse, sqrt(8 / 3))
  expect_equal(joint_mae, 4 / 3)
  expect_identical(joint_n, 3L)
})

test_that("foldwise summaries count only valid correlations", {
  predictions <- data.frame(
    row = 1:8,
    fold = c(1L, 1L, 2L, 2L, 2L, 3L, 3L, 3L),
    observed = c(1, 2, 1, 2, 3, 5, 6, 7),
    joint = c(1, 2, 1, 2, 3, 0, 0, 0),
    positive = c(2, 1, 3, 2, 1, 5, 6, 7),
    negative = c(1, 2, 1, 2, 3, 7, 6, 5)
  )
  result <- structure(
    list(
      spec = spec(),
      settings = list(return_edges = "none"),
      predictions = predictions,
      edges = NULL,
      folds = list(1:2, 3:5, 6:8)
    ),
    class = "cpm"
  )

  metrics <- summary(result)$metrics
  foldwise <- metrics[metrics$level == "foldwise", ]

  expect_equal(
    stats::setNames(foldwise$n_folds, foldwise$prediction),
    c(joint = 1L, positive = 2L, negative = 2L)
  )
  expect_equal(
    summary_correlation_values(metrics, level = "foldwise"),
    c(joint = 1, positive = 0, negative = 0)
  )
  expect_true(is.na(
    summary_correlation_values(
      metrics,
      level = "foldwise",
      field = "std_error"
    )[["joint"]]
  ))
})

test_that("summary.cpm returns the expected object structure", {
  summary_result <- summary(example_summary_result())

  expect_s3_class(summary_result, "cpm_summary")
  expect_identical(
    names(summary_result),
    c("metrics", "tables", "edges", "settings")
  )
  expect_named(
    summary_result$metrics,
    c(
      "level",
      "metric",
      "prediction",
      "estimate",
      "std_error",
      "method",
      "n",
      "n_folds",
      "descriptive_only"
    )
  )
  expect_named(summary_result$tables, c("pooled", "foldwise"))
  expect_identical(summary_result$settings$n_folds, 5L)
})

test_that("summary.cpm returns NA when fewer than three valid pairs remain", {
  sparse_object <- structure(
    list(
      spec = spec(),
      settings = list(
        covariates = FALSE,
        na_action = "fail",
        return_edges = "sum"
      ),
      predictions = data.frame(
        row = 1:3,
        fold = c(1L, 1L, 2L),
        observed = c(NA_real_, 2, NA_real_),
        joint = c(1, 2, 3),
        positive = c(1, 2, 3),
        negative = c(1, 2, 3)
      ),
      edges = matrix(
        c(TRUE, FALSE, FALSE, TRUE),
        ncol = 2,
        dimnames = list(NULL, c("positive", "negative"))
      ),
      folds = list(1:2, 3L)
    ),
    class = "cpm"
  )

  summary_result <- summary(sparse_object)

  expect_true(all(is.na(summary_correlation_values(
    summary_result$metrics,
    level = "pooled"
  ))))
  expect_equal(
    summary_result$edges,
    matrix(
      c(0.5, 0, 0, 0.5),
      ncol = 2,
      dimnames = list(NULL, c("positive", "negative"))
    )
  )
})

test_that("summary.cpm reports pooled and fold-wise correlations and edge rates", {
  predictions <- data.frame(
    row = 1:6,
    fold = c(1L, 1L, 1L, 2L, 2L, 2L),
    observed = c(1, 2, 3, 4, 5, 6),
    joint = c(1, 2, 3, 6, 5, 4),
    positive = c(1, 2, 3, 4, 5, 6),
    negative = c(3, 2, 1, 6, 5, 4)
  )
  folds <- list(1:3, 4:6)
  summary_result <- summary(
    structure(
      list(
        call = quote(cpm(conmat = conmat, behav = behav, spec = spec)),
        spec = spec(),
        settings = list(return_edges = "sum"),
        predictions = predictions,
        edges = matrix(
          c(2, 0, 1, 2),
          ncol = 2,
          dimnames = list(NULL, c("positive", "negative"))
        ),
        folds = folds
      ),
      class = "cpm"
    )
  )
  fold_correlations <- compute_fold_correlation_table(predictions, folds)

  expect_equal(
    summary_correlation_values(
      summary_result$metrics,
      level = "pooled"
    ),
    vapply(
      prediction_streams,
      function(type) safe_cor(predictions$observed, predictions[[type]]),
      numeric(1)
    )
  )
  expect_equal(
    summary_correlation_values(
      summary_result$metrics,
      level = "foldwise"
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
    summary_correlation_values(
      summary_result$metrics,
      level = "foldwise",
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

test_that("summary.cpm returns NULL edges when they were not stored", {
  summary_result <- summary(
    structure(
      list(
        call = quote(cpm(conmat = conmat, behav = behav, spec = spec)),
        spec = spec(),
        settings = list(return_edges = "none"),
        predictions = data.frame(
          row = 1:4,
          fold = c(1L, 1L, 2L, 2L),
          observed = c(NA_real_, NA_real_, NA_real_, NA_real_),
          joint = c(1, 2, 3, 4),
          positive = c(1, 2, 3, 4),
          negative = c(4, 3, 2, 1)
        ),
        edges = NULL,
        folds = list(1:2, 3:4)
      ),
      class = "cpm"
    )
  )

  expect_null(summary_result$edges)
  expect_true(all(is.na(summary_correlation_values(
    summary_result$metrics,
    level = "pooled"
  ))))
  expect_true(all(is.na(summary_correlation_values(
    summary_result$metrics,
    level = "foldwise"
  ))))
})

test_that("summary.cpm keeps explicit LOO summaries usable", {
  withr::local_seed(123)
  conmat <- matrix(rnorm(50), ncol = 10)
  behav <- rnorm(5)

  expect_warning(
    result <- cpm(
      conmat = conmat,
      behav = behav,
      spec = spec(),
      resamples = NULL
    ),
    "assessment folds contain fewer than 3 observations"
  )
  summary_result <- summary(result)
  output <- capture.output(print(summary_result))

  expect_true(all(c("rmse", "mae") %in% summary_result$metrics$metric))
  expect_true(all(
    summary_result$metrics$descriptive_only[
      summary_result$metrics$level == "pooled" &
        summary_result$metrics$metric == "correlation"
    ]
  ))
  pooled_metrics <- tidy(result, component = "metrics", level = "pooled")
  expect_true(all(
    pooled_metrics$descriptive_only[pooled_metrics$metric == "correlation"]
  ))
  expect_false(any(
    pooled_metrics$descriptive_only[pooled_metrics$metric %in% c("rmse", "mae")]
  ))
  expect_true(all(is.finite(summary_correlation_values(
    summary_result$metrics,
    level = "pooled"
  ))))
  expect_true(all(is.na(summary_correlation_values(
    summary_result$metrics,
    level = "foldwise"
  ))))
  expect_true(any(grepl(
    "Pooled correlations \\(Pearson; descriptive only\\):",
    output
  )))
  expect_true(any(grepl("Pooled RMSE:", output, fixed = TRUE)))
  expect_true(any(grepl("Pooled MAE:", output, fixed = TRUE)))
  expect_true(any(grepl("descriptive only", output, fixed = TRUE)))
  expect_true(any(grepl(
    "unavailable because they were undefined for all prediction streams",
    output,
    fixed = TRUE
  )))
})

test_that("summary.cpm supports single-stream net summaries", {
  withr::local_seed(321)
  conmat <- matrix(rnorm(120), nrow = 20)
  behav <- rowMeans(conmat[, 1:5, drop = FALSE]) + rnorm(20, sd = 0.2)

  result <- cpm(
    conmat = conmat,
    behav = behav,
    spec = spec(construction = cpm_construction_strength(sign_mode = "net")),
    resamples = 4
  )
  summary_result <- summary(result)
  output <- capture.output(print(summary_result))

  expect_identical(summary_result$settings$prediction_streams, "net")
  expect_equal(
    length(summary_correlation_values(
      summary_result$metrics,
      level = "pooled",
      prediction_streams = "net"
    )),
    1L
  )
  expect_true(any(grepl("Net:", output, fixed = TRUE)))
})

test_that("summary.cpm supports configurable correlation methods", {
  predictions <- data.frame(
    row = 1:6,
    fold = c(1L, 1L, 1L, 2L, 2L, 2L),
    observed = c(1, 2, 3, 4, 5, 6),
    joint = c(1, 3, 2, 6, 4, 5),
    positive = c(1, 3, 2, 4, 6, 5),
    negative = c(3, 1, 2, 6, 4, 5)
  )
  folds <- list(1:3, 4:6)

  summary_result <- summary(
    structure(
      list(
        call = quote(cpm(conmat = conmat, behav = behav, spec = spec)),
        spec = spec(),
        settings = list(return_edges = "none"),
        predictions = predictions,
        edges = NULL,
        folds = folds
      ),
      class = "cpm"
    ),
    method = "spearman"
  )

  output <- capture.output(print(summary_result))

  expect_identical(summary_result$settings$correlation_method, "spearman")
  expect_true(all(
    summary_result$metrics$method[
      summary_result$metrics$metric == "correlation"
    ] ==
      "spearman"
  ))
  expect_true(any(grepl("Pooled correlations \\(Spearman\\):", output)))
})

test_that("summary.cpm averages fold-wise edges when all edges are stored", {
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
    structure(
      list(
        call = quote(cpm(conmat = conmat, behav = behav, spec = spec)),
        spec = spec(),
        settings = list(return_edges = "all"),
        predictions = data.frame(
          row = 1:4,
          fold = c(1L, 1L, 2L, 2L),
          observed = c(1, 2, 3, 4),
          joint = c(1, 2, 3, 4),
          positive = c(1, 2, 3, 4),
          negative = c(4, 3, 2, 1)
        ),
        edges = stored_edges,
        folds = list(1:2, 3:4)
      ),
      class = "cpm"
    )
  )

  expect_equal(summary_result$edges, apply(stored_edges, c(1, 2), mean))
})

test_that("print.cpm_summary reports fold count, correlations, and edge rates", {
  summary_result <- example_cpm_summary(
    edges = matrix(
      c(0.5, 0.25),
      ncol = 2,
      dimnames = list(NULL, c("positive", "negative"))
    )
  )

  output <- capture.output(print(summary_result))

  expect_true(any(grepl("Number of folds: 5", output, fixed = TRUE)))
  expect_true(any(grepl(
    "Pooled correlations (Pearson):",
    output,
    fixed = TRUE
  )))
  expect_true(any(grepl(
    "Fold-wise correlations (Pearson):",
    output,
    fixed = TRUE
  )))
  expect_true(any(grepl("Joint: 0.400", output, fixed = TRUE)))
  expect_true(any(grepl("Joint: 0.350 (SE 0.050)", output, fixed = TRUE)))
  expect_true(any(grepl("5/5 folds", output, fixed = TRUE)))
  expect_true(any(grepl("Positive: 50.00%", output, fixed = TRUE)))
  expect_true(any(grepl("Negative: 25.00%", output, fixed = TRUE)))
})

test_that("print.cpm_summary omits edge block when edges are not stored", {
  summary_result <- example_cpm_summary()

  output <- capture.output(print(summary_result))

  expect_false(any(grepl("Selected edges", output, fixed = TRUE)))
})

test_that("print.cpm_summary notes when fold-wise correlations are undefined", {
  summary_result <- example_cpm_summary(
    metrics = within(example_cpm_summary()$metrics, {
      estimate[level == "foldwise"] <- NA_real_
      std_error[level == "foldwise"] <- NA_real_
    })
  )

  output <- capture.output(print(summary_result))

  expect_true(any(grepl(
    "unavailable because they were undefined for all prediction streams",
    output
  )))
})

test_that("print.cpm_summary prints fold-wise block when some streams remain estimable", {
  summary_result <- example_cpm_summary(
    metrics = within(example_cpm_summary()$metrics, {
      estimate[level == "foldwise" & prediction == "positive"] <- NA_real_
      std_error[level == "foldwise" & prediction == "positive"] <- NA_real_
      n_folds[level == "foldwise" & prediction == "positive"] <- 0L
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
    c("fold", "metric", "n_assess", "prediction", "estimate", "method")
  )
  expect_equal(nrow(foldwise), 6)
  expect_equal(sort(unique(foldwise$fold)), 1:2)
  expect_equal(unique(foldwise$n_assess), 3)
})

test_that("summary metrics combine errors and fold-wise correlations", {
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

  pooled$metric <- "correlation"
  pooled$n <- 10L
  pooled$descriptive_only <- FALSE
  foldwise$metric <- "correlation"
  metrics <- compute_summary_metrics(
    pooled_metrics = pooled,
    foldwise_metrics = foldwise,
    correlation_method = "pearson"
  )

  expect_named(
    metrics,
    c(
      "level",
      "metric",
      "prediction",
      "estimate",
      "std_error",
      "method",
      "n",
      "n_folds",
      "descriptive_only"
    )
  )
  expect_true(all(c("pooled", "foldwise") %in% metrics$level))
  expect_true(all(metrics$metric == "correlation"))
  expect_equal(
    summary_correlation_values(metrics, level = "pooled"),
    c(joint = 0.4, positive = 0.2, negative = -0.1)
  )
  expect_equal(
    summary_correlation_values(metrics, level = "foldwise"),
    c(joint = 0.3, positive = 0.2, negative = -0.1)
  )
})
