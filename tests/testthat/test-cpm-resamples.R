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

test_that("safe_cor returns NA for degenerate vectors", {
  safe_cor <- getFromNamespace("safe_cor", "cpmr")

  expect_true(is.na(safe_cor(c(1, 1, 1), c(1, 2, 3))))
  expect_true(is.na(safe_cor(c(1), c(1))))
})

test_that("print.cpm_resamples prints NA instead of NaN for all-NA metrics", {
  x <- structure(
    list(
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
  expect_true(any(grepl("Both: NA", out, fixed = TRUE)))
})
