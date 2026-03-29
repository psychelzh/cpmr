prediction_matrix <- function(x) {
  as.matrix(x$predictions[, prediction_columns(x$predictions), drop = FALSE])
}

prediction_complete_cases <- function(x) {
  stats::complete.cases(
    x$predictions[, prediction_columns(x$predictions), drop = FALSE]
  )
}

example_cpm_result <- function(
  spec_obj = spec(),
  conmat,
  behav,
  covariates = NULL,
  resamples = 5,
  return_edges = "sum",
  na_action = "fail"
) {
  cpm(
    conmat = conmat,
    behav = behav,
    spec = spec_obj,
    covariates = covariates,
    resamples = resamples,
    return_edges = return_edges,
    na_action = na_action
  )
}

test_that("cpm returns a fold-based cpm object", {
  withr::local_seed(123)
  conmat <- matrix(rnorm(100), ncol = 10)
  behav <- rnorm(10)

  result <- example_cpm_result(conmat = conmat, behav = behav)

  expect_s3_class(result, "cpm")
  expect_length(result$folds, 5)
  expect_named(
    result$predictions,
    c("row", "fold", "observed", "joint", "positive", "negative")
  )
  expect_equal(dim(result$edges), c(ncol(conmat), 2))
  expect_identical(result$settings$return_edges, "sum")
  expect_identical(result$settings$na_action, "fail")
  expect_false(result$settings$covariates)
})

test_that("cpm works with alternative staged settings", {
  withr::local_seed(123)
  conmat <- matrix(rnorm(100), ncol = 10)
  behav <- rnorm(10)

  result <- example_cpm_result(
    spec_obj = spec(
      selection = cpm_selection_cor(criterion = "proportion"),
      construction = cpm_construction_strength(sign_mode = "net")
    ),
    conmat = conmat,
    behav = behav
  )

  expect_named(result$predictions, c("row", "fold", "observed", "net"))
  expect_true(any(prediction_complete_cases(result)))
  expect_identical(result$spec$selection$criterion, "proportion")
  expect_identical(result$spec$construction$sign_mode, "net")
})

test_that("cpm works with covariates", {
  withr::local_seed(123)
  conmat <- matrix(rnorm(100), ncol = 10)
  behav <- rnorm(10)
  covariates <- matrix(rnorm(10), ncol = 1)

  result <- example_cpm_result(
    conmat = conmat,
    behav = behav,
    covariates = covariates
  )

  expect_true(isTRUE(result$settings$covariates))
  expect_equal(nrow(result$predictions), 10)
  expect_true(any(prediction_complete_cases(result)))
})

test_that("cpm keeps behavior names in prediction row names", {
  withr::local_seed(123)
  conmat <- matrix(rnorm(100), ncol = 10)
  behav <- rnorm(10)
  names(behav) <- LETTERS[1:10]

  result <- example_cpm_result(
    conmat = conmat,
    behav = behav
  )

  expect_identical(rownames(result$predictions), LETTERS[1:10])
  expect_equal(result$predictions$row, seq_along(behav))
})

test_that("cpm accepts matrix behavior and vector covariates", {
  withr::local_seed(123)
  conmat <- matrix(rnorm(100), ncol = 10)
  behav <- rnorm(10)
  covariates <- matrix(rnorm(10), ncol = 1)
  folds <- list(1:2, 3:4, 5:6, 7:8, 9:10)

  vector_result <- example_cpm_result(
    conmat = conmat,
    behav = behav,
    covariates = covariates,
    resamples = folds
  )

  expect_identical(
    example_cpm_result(
      conmat = conmat,
      behav = matrix(behav, ncol = 1),
      covariates = covariates,
      resamples = folds
    )[c("predictions", "edges", "folds")],
    vector_result[c("predictions", "edges", "folds")]
  )
  expect_identical(
    example_cpm_result(
      conmat = conmat,
      behav = matrix(behav, nrow = 1),
      covariates = covariates,
      resamples = folds
    )[c("predictions", "edges", "folds")],
    vector_result[c("predictions", "edges", "folds")]
  )
  expect_identical(
    example_cpm_result(
      conmat = conmat,
      behav = behav,
      covariates = drop(covariates),
      resamples = folds
    )[c("predictions", "edges", "folds")],
    vector_result[c("predictions", "edges", "folds")]
  )
})

test_that("cpm throws informative data-checking errors", {
  withr::local_seed(123)
  conmat <- matrix(rnorm(100), ncol = 10)

  expect_error(
    example_cpm_result(
      conmat = conmat,
      behav = matrix(rnorm(20), ncol = 2)
    ),
    "Behavior data must be a numeric vector."
  )
  expect_error(
    example_cpm_result(
      conmat = conmat,
      behav = rnorm(20)
    ),
    "The number of observations in `conmat` and `behav` must match."
  )
  expect_error(
    example_cpm_result(
      conmat = conmat,
      behav = rnorm(10),
      covariates = matrix(rnorm(20), ncol = 1)
    ),
    "The number of observations in `covariates` and `behav` must match."
  )
})

test_that("cpm supports missing-data exclusion while keeping row layout", {
  withr::local_seed(123)
  conmat <- matrix(rnorm(120), ncol = 12)
  behav <- rnorm(10)
  covariates <- matrix(rnorm(20), ncol = 2)

  behav[2] <- NA
  conmat[4, 3] <- NA
  covariates[6, 1] <- NA

  include_cases <- setdiff(seq_len(10), c(2L, 4L, 6L))
  resamples <- list(include_cases[1:3], include_cases[4:5], include_cases[6:7])

  result <- example_cpm_result(
    conmat = conmat,
    behav = behav,
    covariates = covariates,
    resamples = resamples,
    na_action = "exclude"
  )

  expect_identical(result$folds, resamples)
  expect_equal(
    sort(result$predictions$row[!is.na(result$predictions$fold)]),
    include_cases
  )
  expect_true(all(is.na(prediction_matrix(result)[-include_cases, ])))
})

example_cpm_output_result <- function(resamples = 5L) {
  withr::local_seed(123)
  conmat <- matrix(rnorm(120), ncol = 12)
  behav <- rnorm(10)

  cpm(conmat = conmat, behav = behav, spec = spec(), resamples = resamples)
}

test_that("print.cpm reports summary fields", {
  res <- example_cpm_output_result()

  expect_output(print(res), "CPM")
  expect_output(print(res), "Complete cases")
  expect_output(print(res), "Covariates:\\s+none")
  expect_output(print(res), "Missing data:\\s+fail")
  expect_output(print(res), "Edge storage:\\s+summed across folds")
  expect_output(print(res), "Selection:\\s+pearson / p_value / 0.01")
  expect_output(
    print(res),
    "Construction:\\s+separate / no weighting / raw edges"
  )
  expect_output(print(res), "Model:\\s+linear regression")
  expect_output(print(res), "Use summary\\(\\) for aggregate correlations")
})

test_that("print.cpm uses human-readable edge storage labels", {
  x <- structure(
    list(
      call = quote(cpm(conmat = conmat, behav = behav, spec = spec())),
      spec = spec(),
      folds = list(1:3, 4:6),
      predictions = data.frame(
        row = 1:6,
        fold = c(1L, 1L, 1L, 2L, 2L, 2L),
        observed = c(1, 2, 3, 4, 5, 6),
        joint = c(1, 2, 3, 6, 5, 4),
        positive = c(1, 2, 3, 4, 5, 6),
        negative = c(3, 2, 1, 6, 5, 4)
      ),
      settings = list(
        covariates = FALSE,
        na_action = "fail",
        return_edges = "sum"
      ),
      edges = matrix(
        c(2, 0, 1, 2),
        ncol = 2,
        dimnames = list(NULL, c("positive", "negative"))
      )
    ),
    class = "cpm"
  )

  out <- capture.output(print(x))
  expect_true(any(grepl(
    "Edge storage:\\s+summed across folds",
    out
  )))
})

test_that("print.cpm shows compact weighted construction summary", {
  x <- structure(
    list(
      call = quote(cpm(conmat = conmat, behav = behav, spec = spec())),
      spec = spec(
        construction = cpm_construction_strength(
          sign_mode = "separate",
          weight_scale = 0.03,
          standardize_edges = TRUE
        )
      ),
      folds = list(1:3, 4:6),
      predictions = data.frame(
        row = 1:6,
        fold = c(1L, 1L, 1L, 2L, 2L, 2L),
        observed = c(1, 2, 3, 4, 5, 6),
        joint = c(1, 2, 3, 6, 5, 4),
        positive = c(1, 2, 3, 4, 5, 6),
        negative = c(3, 2, 1, 6, 5, 4)
      ),
      settings = list(
        covariates = FALSE,
        na_action = "fail",
        return_edges = "none"
      ),
      edges = NULL
    ),
    class = "cpm"
  )

  expect_output(
    print(x),
    "Construction:\\s+separate / sigmoid weighting \\(0.03\\) / z-score edges"
  )
})

test_that("tidy metrics returns pooled and foldwise metric tables", {
  res <- example_cpm_output_result()

  foldwise <- tidy(res, component = "metrics")
  pooled <- tidy(res, component = "metrics", level = "pooled")

  expect_true(all(
    c(
      "fold",
      "n_assess",
      "prediction",
      "estimate",
      "method"
    ) %in%
      names(foldwise)
  ))
  expect_true(all(
    c(
      "prediction",
      "estimate",
      "method"
    ) %in%
      names(pooled)
  ))
  expect_true(all(c("joint", "positive", "negative") %in% pooled$prediction))
  expect_true(all(pooled$method == "pearson"))
})

test_that("tidy metrics supports spearman correlation", {
  res <- example_cpm_output_result()

  pooled <- tidy(
    res,
    component = "metrics",
    level = "pooled",
    method = "spearman"
  )

  expect_equal(nrow(pooled), 3)
  expect_true(all(pooled$method == "spearman"))
})

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

example_tidy_result <- function() {
  withr::local_seed(123)
  conmat <- matrix(rnorm(200), nrow = 20)
  behav <- rnorm(20)

  cpm(conmat = conmat, behav = behav, spec = spec(), resamples = 4)
}

test_that("Basic case works for `tidy()`", {
  result <- example_tidy_result()

  pooled <- tidy(result, component = "metrics", level = "pooled")
  edges <- tidy(result, component = "edges")

  expect_named(pooled, c("prediction", "estimate", "method"))
  expect_true(all(c("joint", "positive", "negative") %in% pooled$prediction))
  expect_named(
    edges,
    c(
      "positive",
      "negative"
    )
  )
})

test_that("Support pass arguments of `summary()`", {
  result <- example_tidy_result()

  expect_equal(
    unique(
      tidy(
        result,
        component = "metrics",
        level = "pooled",
        method = "spearman"
      )$method
    ),
    "spearman"
  )
  expect_named(
    tidy(result, component = "edges"),
    c("positive", "negative")
  )
})

test_that("tidy supports pooled and foldwise metric tables", {
  result <- example_tidy_result()

  foldwise <- tidy(result, component = "metrics")
  pooled <- tidy(result, component = "metrics", level = "pooled")

  expect_true(all(
    c("fold", "n_assess", "prediction", "estimate", "method") %in%
      names(foldwise)
  ))
  expect_true(all(c("prediction", "estimate", "method") %in% names(pooled)))
  expect_true(all(pooled$method == "pearson"))
})

test_that("tidy edges errors clearly when edge storage is disabled", {
  withr::local_seed(123)
  conmat <- matrix(rnorm(100), ncol = 10)
  behav <- rnorm(10)
  result <- cpm(
    conmat = conmat,
    behav = behav,
    spec = spec(),
    resamples = 5,
    return_edges = "none"
  )

  expect_error(
    tidy(result, component = "edges"),
    paste0(
      "Edge output is unavailable because this `cpm` object was fit ",
      "with `return_edges = \"none\"`. Refit with `return_edges = ",
      "\"sum\"` or `\"all\"` to tidy edges."
    ),
    fixed = TRUE
  )
})
