example_cpm_result <- function(resamples = 5L) {
  withr::local_seed(123)
  conmat <- matrix(rnorm(120), ncol = 12)
  behav <- rnorm(10)

  cpm(conmat = conmat, behav = behav, spec = spec(), resamples = resamples)
}

test_that("print.cpm reports summary fields", {
  res <- example_cpm_result()

  expect_output(print(res), "CPM")
  expect_output(print(res), "Complete cases")
  expect_output(print(res), "Covariates:\\s+none")
  expect_output(print(res), "Missing data:\\s+fail")
  expect_output(print(res), "Edge storage:\\s+summed across folds")
  expect_output(print(res), "Selection method:\\s+pearson")
  expect_output(print(res), "Construction sign mode:\\s+separate")
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

test_that("tidy metrics returns pooled and foldwise metric tables", {
  res <- example_cpm_result()

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
  res <- example_cpm_result()

  pooled <- tidy(
    res,
    component = "metrics",
    level = "pooled",
    method = "spearman"
  )

  expect_equal(nrow(pooled), 3)
  expect_true(all(pooled$method == "spearman"))
})
