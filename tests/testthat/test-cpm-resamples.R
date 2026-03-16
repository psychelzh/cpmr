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
    folds = list(1:2)
  )

  expect_s3_class(resamples_object, "cpm_resamples")
  expect_identical(as.character(resamples_object$call[[1]]), "fit_resamples")
  expect_identical(resamples_object$predictions, predictions)
})

test_that("print.cpm_resamples prints NA instead of NaN for all-NA metrics", {
  x <- structure(
    list(
      call = quote(fit_resamples(spec, conmat = conmat, behav = behav)),
      folds = list(1:2, 3:4),
      predictions = data.frame(
        row = 1:4,
        fold = c(1L, 1L, 2L, 2L),
        real = c(NA_real_, NA_real_, NA_real_, NA_real_),
        both = c(1, 2, 3, 4),
        pos = c(1, 2, 3, 4),
        neg = c(1, 2, 3, 4)
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
      folds = list(1:3, 4:6),
      predictions = data.frame(
        row = 1:6,
        fold = c(1L, 1L, 1L, 2L, 2L, 2L),
        real = c(1, 2, 3, 4, 5, 6),
        both = c(1, 2, 3, 6, 5, 4),
        pos = c(1, 2, 3, 4, 5, 6),
        neg = c(3, 2, 1, 6, 5, 4)
      ),
      params = list(return_edges = "sum")
    ),
    class = "cpm_resamples"
  )

  out <- capture.output(print(x))
  expect_true(any(grepl("Combined: 0.000", out, fixed = TRUE)))
  expect_true(any(grepl("Positive: 1.000", out, fixed = TRUE)))
  expect_true(any(grepl("Negative: -1.000", out, fixed = TRUE)))
})
