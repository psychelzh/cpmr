prediction_streams <- c("joint", "positive", "negative")

example_summary_result <- function() {
  withr::local_seed(123)
  conmat <- matrix(rnorm(10000), nrow = 10)
  behav <- rnorm(10)

  cpm(conmat = conmat, behav = behav, spec = spec(), resamples = 5)
}

example_cpm_summary <- function(
  metrics = rbind(
    data.frame(
      level = "pooled",
      prediction = prediction_streams,
      estimate = c(0.4, 0.2, -0.1),
      std_error = NA_real_,
      method = "pearson",
      stringsAsFactors = FALSE
    ),
    data.frame(
      level = "foldwise",
      prediction = prediction_streams,
      estimate = c(0.35, 0.15, -0.05),
      std_error = c(0.05, 0.02, 0.01),
      method = "pearson",
      stringsAsFactors = FALSE
    )
  ),
  tables = list(
    pooled = data.frame(
      prediction = prediction_streams,
      estimate = c(0.4, 0.2, -0.1),
      method = "pearson",
      stringsAsFactors = FALSE
    ),
    foldwise = data.frame(
      fold = rep(1:5, each = 3),
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
    edge_storage = "not stored",
    correlation_method = "pearson",
    prediction_streams = prediction_streams
  )
) {
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

test_that("summary.cpm returns the expected object structure", {
  summary_result <- summary(example_summary_result())

  expect_s3_class(summary_result, "cpm_summary")
  expect_identical(
    names(summary_result),
    c("metrics", "tables", "edges", "settings")
  )
  expect_named(
    summary_result$metrics,
    c("level", "prediction", "estimate", "std_error", "method")
  )
  expect_named(summary_result$tables, c("pooled", "foldwise"))
  expect_identical(summary_result$settings$n_folds, 5L)
})

test_that("summary.cpm returns NA when fewer than two valid pairs remain", {
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

test_that("summary.cpm keeps default LOO summaries usable", {
  withr::local_seed(123)
  conmat <- matrix(rnorm(50), ncol = 10)
  behav <- rnorm(5)

  result <- cpm(conmat = conmat, behav = behav, spec = spec())
  summary_result <- summary(result)
  output <- capture.output(print(summary_result))

  expect_true(all(is.finite(summary_correlation_values(
    summary_result$metrics,
    level = "pooled"
  ))))
  expect_true(all(is.na(summary_correlation_values(
    summary_result$metrics,
    level = "foldwise"
  ))))
  expect_true(any(grepl("Pooled correlations \\(Pearson\\):", output)))
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
    spec = spec(construction = cpm_construction_summary(sign_mode = "net")),
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
  expect_true(all(summary_result$metrics$method == "spearman"))
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
