example_resample_result <- function(kfolds = 5L) {
  withr::local_seed(123)
  conmat <- matrix(rnorm(120), ncol = 12)
  behav <- rnorm(10)

  fit_resamples(cpm_spec(), conmat = conmat, behav = behav, kfolds = kfolds)
}

test_that("print.cpm_resamples reports summary fields", {
  res <- example_resample_result()

  expect_output(print(res), "CPM resamples")
  expect_output(print(res), "Complete cases")
  expect_output(print(res), "Edge storage: not stored")
  expect_output(print(res), "Use summary\\(\\) for aggregate metrics")
})

test_that("new_cpm_resamples builds resampling objects", {
  spec <- cpm_spec()
  edges <- matrix(
    c(TRUE, FALSE, FALSE, TRUE),
    ncol = 2,
    dimnames = list(NULL, c("positive", "negative"))
  )
  predictions <- data.frame(
    row = 1:2,
    fold = 1L,
    observed = c(1, 2),
    joint = c(1.1, 1.9),
    positive = c(1.0, 2.0),
    negative = c(0.9, 2.1)
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

test_that("print.cpm_resamples uses human-readable edge storage labels", {
  x <- structure(
    list(
      call = quote(fit_resamples(spec, conmat = conmat, behav = behav)),
      folds = list(1:3, 4:6),
      predictions = data.frame(
        row = 1:6,
        fold = c(1L, 1L, 1L, 2L, 2L, 2L),
        observed = c(1, 2, 3, 4, 5, 6),
        joint = c(1, 2, 3, 6, 5, 4),
        positive = c(1, 2, 3, 4, 5, 6),
        negative = c(3, 2, 1, 6, 5, 4)
      ),
      params = list(return_edges = "sum")
    ),
    class = "cpm_resamples"
  )

  out <- capture.output(print(x))
  expect_true(any(grepl(
    "Edge storage: summed across folds",
    out,
    fixed = TRUE
  )))
})

test_that("resample_metrics returns pooled and foldwise metrics", {
  res <- example_resample_result()

  foldwise <- resample_metrics(res)
  pooled <- resample_metrics(res, level = "pooled")

  expect_named(
    foldwise,
    c("fold", "n_assess", "metric", "prediction", "estimate")
  )
  expect_named(pooled, c("metric", "prediction", "estimate"))
  expect_true(all(c("rmse", "mae", "correlation") %in% pooled$metric))
  expect_true(all(c("joint", "positive", "negative") %in% pooled$prediction))
})

test_that("resample_metrics supports metric filtering and spearman correlation", {
  res <- example_resample_result()

  pooled <- resample_metrics(
    res,
    level = "pooled",
    metrics = "correlation",
    correlation_method = "spearman"
  )

  expect_true(all(pooled$metric == "correlation"))
  expect_equal(nrow(pooled), 3)
})

test_that("resample_metrics ignores correlation_method when correlation is absent", {
  res <- example_resample_result()

  metrics <- resample_metrics(
    res,
    metrics = "rmse",
    correlation_method = "kendall"
  )

  expect_true(all(metrics$metric == "rmse"))
  expect_equal(nrow(metrics), 15)
})

test_that("resample_metrics validates object type", {
  expect_error(
    resample_metrics(summary(fit(
      cpm_spec(),
      matrix(rnorm(100), ncol = 10),
      rnorm(10)
    ))),
    "`resample_metrics()` requires a `cpm_resamples` object.",
    fixed = TRUE
  )
})
