example_resample_result <- function(resamples = 5L) {
  withr::local_seed(123)
  conmat <- matrix(rnorm(120), ncol = 12)
  behav <- rnorm(10)

  cpm(conmat = conmat, behav = behav, spec = spec(), resamples = resamples)
}

single_fit_result <- function(
  spec = spec(),
  conmat,
  behav,
  covariates = NULL,
  na_action = "fail"
) {
  run_single_fit(
    object = spec,
    conmat = conmat,
    behav = behav,
    covariates = covariates,
    na_action = na_action,
    call = quote(cpm(conmat = conmat, behav = behav, spec = spec))
  )
}

test_that("print.cpm reports summary fields", {
  res <- example_resample_result()

  expect_output(print(res), "CPM")
  expect_output(print(res), "Complete cases")
  expect_output(print(res), "Covariates:\\s+none")
  expect_output(print(res), "Missing data:\\s+fail")
  expect_output(print(res), "Edge storage:\\s+summed across folds")
  expect_output(print(res), "Selection method:\\s+pearson")
  expect_output(print(res), "Construction sign mode:\\s+separate")
  expect_output(print(res), "Use summary\\(\\) for aggregate metrics")
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

test_that("tidy metrics returns pooled and foldwise metric tables", {
  res <- example_resample_result()

  foldwise <- tidy(res, component = "metrics")
  pooled <- tidy(res, component = "metrics", level = "pooled")

  expect_true(all(
    c(
      "fold",
      "n_assess",
      "metric",
      "prediction",
      "estimate"
    ) %in%
      names(foldwise)
  ))
  expect_true(all(
    c(
      "metric",
      "prediction",
      "estimate"
    ) %in%
      names(pooled)
  ))
  expect_true(all(c("rmse", "mae", "correlation") %in% pooled$metric))
  expect_true(all(c("joint", "positive", "negative") %in% pooled$prediction))
})

test_that("tidy metrics supports metric filtering and spearman correlation", {
  res <- example_resample_result()

  pooled <- tidy(
    res,
    component = "metrics",
    level = "pooled",
    metrics = "correlation",
    method = "spearman"
  )

  expect_true(all(pooled$metric == "correlation"))
  expect_equal(nrow(pooled), 3)
})

test_that("tidy metrics can return non-correlation metrics only", {
  res <- example_resample_result()

  metrics <- tidy(
    res,
    component = "metrics",
    metrics = "rmse"
  )

  expect_true(all(metrics$metric == "rmse"))
  expect_equal(nrow(metrics), 15)
})
