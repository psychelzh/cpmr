test_that("print.cpm_resamples reports summary fields", {
  withr::local_seed(123)
  conmat <- matrix(rnorm(120), ncol = 12)
  behav <- rnorm(10)
  spec <- cpm_spec()

  res <- fit_resamples(spec, conmat = conmat, behav = behav, kfolds = 5)

  expect_output(print(res), "CPM resample results")
  expect_output(print(res), "Mean correlations")
  expect_output(print(res), "Edge storage")
})

test_that("new_cpm_resamples builds resampling objects", {
  spec <- cpm_spec()
  edges <- matrix(
    c(TRUE, FALSE, FALSE, TRUE),
    ncol = 2,
    dimnames = list(NULL, c("pos", "neg"))
  )
  metrics <- data.frame(
    fold = 1L,
    n_assess = 2L,
    both = 0.5,
    pos = 0.4,
    neg = 0.3
  )
  predictions <- data.frame(
    row = 1:2,
    fold = 1L,
    real = c(1, 2),
    both = c(1.1, 1.9),
    pos = c(1.0, 2.0),
    neg = c(0.9, 2.1)
  )

  resamples_object <- new_cpm_resamples(
    call = quote(fit_resamples(spec, conmat = conmat, behav = behav)),
    spec = spec,
    params = list(return_edges = "sum"),
    predictions = predictions,
    edges = edges,
    folds = list(1:2),
    metrics = metrics
  )

  expect_s3_class(resamples_object, "cpm_resamples")
  expect_identical(as.character(resamples_object$call[[1]]), "fit_resamples")
  expect_identical(resamples_object$metrics, metrics)
  expect_identical(resamples_object$predictions, predictions)
})

test_that("compute_fold_metrics summarizes each assessment fold", {
  real <- c(1, 2, 3, 4)
  pred <- cbind(
    both = c(1, 2, 3, 4),
    pos = c(1, 2, 3, 4),
    neg = c(4, 3, 2, 1)
  )
  folds <- list(1:2, 3:4)

  metrics <- compute_fold_metrics(real, pred, folds)

  expect_named(metrics, c("fold", "n_assess", "both", "pos", "neg"))
  expect_equal(metrics$fold, 1:2)
  expect_equal(metrics$n_assess, c(2, 2))
  expect_true(all(is.finite(metrics$both[1:2])))
  expect_true(all(is.finite(metrics$pos[1:2])))
})

test_that("compute_fold_predictions annotates rows with fold ids", {
  real <- c(1, 2, 3, 4)
  pred <- cbind(
    both = c(1.1, 2.1, 3.1, 4.1),
    pos = c(1.2, 2.2, 3.2, 4.2),
    neg = c(0.9, 1.9, 2.9, 3.9)
  )
  folds <- list(c(2L, 4L), c(1L, 3L))

  collected <- compute_fold_predictions(real, pred, folds)

  expect_named(collected, c("row", "fold", "real", "both", "pos", "neg"))
  expect_equal(collected$row, 1:4)
  expect_equal(collected$fold, c(2L, 1L, 2L, 1L))
  expect_equal(collected$real, real)
})

test_that("print.cpm_resamples prints NA instead of NaN for all-NA metrics", {
  x <- structure(
    list(
      call = quote(fit_resamples(spec, conmat = conmat, behav = behav)),
      folds = list(1:2, 3:4),
      predictions = data.frame(row = 1:4, fold = c(1, 1, 2, 2)),
      metrics = data.frame(
        fold = 1:2,
        n_assess = c(2, 2),
        both = c(NA_real_, NA_real_),
        pos = c(NA_real_, NA_real_),
        neg = c(NA_real_, NA_real_)
      ),
      params = list(return_edges = "none")
    ),
    class = "cpm_resamples"
  )

  out <- capture.output(print(x))
  expect_false(any(grepl("NaN", out, fixed = TRUE)))
  expect_true(any(grepl("Combined: NA", out, fixed = TRUE)))
})

test_that("print.cpm_resamples computes finite means when available", {
  x <- structure(
    list(
      call = quote(fit_resamples(spec, conmat = conmat, behav = behav)),
      folds = list(1:2, 3:4),
      predictions = data.frame(row = 1:4, fold = c(1, 1, 2, 2)),
      metrics = data.frame(
        fold = 1:2,
        n_assess = c(2, 2),
        both = c(0.5, 0.25),
        pos = c(0.2, 0.4),
        neg = c(-0.1, -0.2)
      ),
      params = list(return_edges = "sum")
    ),
    class = "cpm_resamples"
  )

  out <- capture.output(print(x))
  expect_true(any(grepl("Combined: 0.375", out, fixed = TRUE)))
  expect_true(any(grepl("Positive: 0.300", out, fixed = TRUE)))
  expect_true(any(grepl("Negative: -0.150", out, fixed = TRUE)))
})
