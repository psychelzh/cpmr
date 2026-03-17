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
        both = c(1, 2, 3),
        pos = c(1, 2, 3),
        neg = c(1, 2, 3)
      ),
      edges = matrix(
        c(TRUE, FALSE, FALSE, TRUE),
        ncol = 2,
        dimnames = list(NULL, c("pos", "neg"))
      ),
      params = list()
    ),
    class = "cpm"
  )

  summary_result <- summary(sparse_object)

  expect_true(all(is.na(summary_result$performance)))
  expect_identical(summary_result$edges, sparse_object$edges)
})

test_that("print.cpm_summary reports NA edge rates when stored edges are all missing", {
  summary_result <- structure(
    list(
      performance = matrix(
        c(0.1, 0.2, 0.3),
        nrow = 1,
        dimnames = list(NULL, c("both", "pos", "neg"))
      ),
      edges = matrix(
        c(NA, NA),
        ncol = 2,
        dimnames = list(NULL, c("pos", "neg"))
      ),
      params = list(method = "pearson")
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
    both = c(1, 2, 3, 6, 5, 4),
    pos = c(1, 2, 3, 4, 5, 6),
    neg = c(3, 2, 1, 6, 5, 4)
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
        dimnames = list(NULL, c("pos", "neg"))
      ),
      folds = folds
    )
  )
  fold_correlations <- compute_fold_correlations(predictions, folds)

  expect_s3_class(summary_result, "cpm_resamples_summary")
  expect_identical(
    colnames(summary_result$errors),
    c("both", "pos", "neg")
  )
  expect_identical(rownames(summary_result$errors), c("rmse", "mae"))
  expect_equal(
    summary_result$errors["rmse", ],
    vapply(
      prediction_types,
      function(type) safe_rmse(predictions$real, predictions[[type]]),
      numeric(1)
    )
  )
  expect_equal(
    summary_result$errors["mae", ],
    vapply(
      prediction_types,
      function(type) safe_mae(predictions$real, predictions[[type]]),
      numeric(1)
    )
  )
  expect_equal(
    summary_result$pooled_correlation,
    vapply(
      prediction_types,
      function(type) safe_cor(predictions$real, predictions[[type]]),
      numeric(1)
    )
  )
  expect_equal(
    summary_result$foldwise_correlation["mean", ],
    vapply(
      prediction_types,
      function(type) safe_mean(fold_correlations[[type]]),
      numeric(1)
    )
  )
  expect_equal(
    summary_result$foldwise_correlation["std_error", ],
    vapply(
      prediction_types,
      function(type) safe_std_error(fold_correlations[[type]]),
      numeric(1)
    )
  )
  expect_equal(summary_result$edges[, "pos"], c(1, 0))
  expect_equal(summary_result$edges[, "neg"], c(0.5, 1))
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
        both = c(1, 2, 3, 4),
        pos = c(1, 2, 3, 4),
        neg = c(4, 3, 2, 1)
      ),
      edges = NULL,
      folds = list(1:2, 3:4)
    )
  )

  expect_null(summary_result$edges)
  expect_true(all(is.na(summary_result$pooled_correlation)))
  expect_true(all(is.na(summary_result$foldwise_correlation)))
})

test_that("summary.cpm_resamples keeps default LOO summaries usable", {
  withr::local_seed(123)
  conmat <- matrix(rnorm(50), ncol = 10)
  behav <- rnorm(5)

  result <- fit_resamples(cpm_spec(), conmat = conmat, behav = behav)
  summary_result <- summary(result)

  expect_true(all(is.finite(summary_result$errors[, prediction_types])))
  expect_true(all(is.na(summary_result$foldwise_correlation)))

  output <- capture.output(print(summary_result))
  expect_true(any(grepl("Pooled correlations", output, fixed = TRUE)))
  expect_true(any(grepl(
    "unavailable because they were undefined for all prediction streams",
    output,
    fixed = TRUE
  )))
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
    dimnames = list(NULL, c("pos", "neg"), NULL)
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
        both = c(1, 2, 3, 4),
        pos = c(1, 2, 3, 4),
        neg = c(4, 3, 2, 1)
      ),
      edges = stored_edges,
      folds = list(1:2, 3:4)
    )
  )

  expect_equal(summary_result$edges, apply(stored_edges, c(1, 2), mean))
})

test_that("print.cpm_resamples_summary reports fold count and rates", {
  summary_result <- structure(
    list(
      errors = rbind(
        rmse = c(both = 0.8, pos = 0.9, neg = 1.0),
        mae = c(both = 0.6, pos = 0.7, neg = 0.8)
      ),
      pooled_correlation = c(both = 0.4, pos = 0.2, neg = -0.1),
      foldwise_correlation = rbind(
        mean = c(both = 0.35, pos = 0.15, neg = -0.05),
        std_error = c(both = 0.05, pos = 0.02, neg = 0.01)
      ),
      edges = matrix(
        c(0.5, 0.25),
        ncol = 2,
        dimnames = list(NULL, c("pos", "neg"))
      ),
      params = list(kfolds = 5L, return_edges = "sum")
    ),
    class = "cpm_resamples_summary"
  )

  output <- capture.output(print(summary_result))

  expect_true(any(grepl("Number of folds: 5", output, fixed = TRUE)))
  expect_true(any(grepl("RMSE", output, fixed = TRUE)))
  expect_true(any(grepl("Combined: 0.400", output, fixed = TRUE)))
  expect_true(any(grepl("Combined: 0.350 (SE 0.050)", output, fixed = TRUE)))
  expect_true(any(grepl("Positive: 50.00%", output, fixed = TRUE)))
  expect_true(any(grepl("Negative: 25.00%", output, fixed = TRUE)))
})

test_that("print.cpm_resamples_summary omits edge block when edges are not stored", {
  summary_result <- structure(
    list(
      errors = rbind(
        rmse = c(both = 0.8, pos = 0.9, neg = 1.0),
        mae = c(both = 0.6, pos = 0.7, neg = 0.8)
      ),
      pooled_correlation = c(both = 0.4, pos = 0.2, neg = -0.1),
      foldwise_correlation = rbind(
        mean = c(both = 0.4, pos = 0.2, neg = -0.1),
        std_error = c(both = 0.05, pos = 0.02, neg = 0.01)
      ),
      edges = NULL,
      params = list(kfolds = 5L, return_edges = "none")
    ),
    class = "cpm_resamples_summary"
  )

  output <- capture.output(print(summary_result))

  expect_false(any(grepl("Selected edges", output, fixed = TRUE)))
})

test_that("print.cpm_resamples_summary notes when fold-wise correlations are undefined", {
  summary_result <- structure(
    list(
      errors = rbind(
        rmse = c(both = 0.8, pos = 0.9, neg = 1.0),
        mae = c(both = 0.6, pos = 0.7, neg = 0.8)
      ),
      pooled_correlation = c(both = 0.4, pos = 0.2, neg = -0.1),
      foldwise_correlation = rbind(
        mean = c(both = NA_real_, pos = NA_real_, neg = NA_real_),
        std_error = c(both = NA_real_, pos = NA_real_, neg = NA_real_)
      ),
      edges = NULL,
      params = list(kfolds = 5L, return_edges = "none")
    ),
    class = "cpm_resamples_summary"
  )

  output <- capture.output(print(summary_result))

  expect_true(any(grepl(
    "unavailable because they were undefined for all prediction streams",
    output
  )))
})

test_that("print.cpm_resamples_summary prints fold-wise block when some streams remain estimable", {
  summary_result <- structure(
    list(
      errors = rbind(
        rmse = c(both = 0.8, pos = 0.9, neg = 1.0),
        mae = c(both = 0.6, pos = 0.7, neg = 0.8)
      ),
      pooled_correlation = c(both = 0.4, pos = 0.2, neg = -0.1),
      foldwise_correlation = rbind(
        mean = c(both = 0.35, pos = NA_real_, neg = -0.05),
        std_error = c(both = 0.05, pos = NA_real_, neg = 0.01)
      ),
      edges = NULL,
      params = list(kfolds = 5L, return_edges = "none")
    ),
    class = "cpm_resamples_summary"
  )

  output <- capture.output(print(summary_result))

  expect_true(any(grepl("Fold-wise correlations:", output, fixed = TRUE)))
  expect_true(any(grepl("Combined: 0.350 (SE 0.050)", output, fixed = TRUE)))
  expect_true(any(grepl("Positive: NA", output, fixed = TRUE)))
  expect_false(any(grepl(
    "unavailable because they were undefined for all prediction streams",
    output,
    fixed = TRUE
  )))
})
