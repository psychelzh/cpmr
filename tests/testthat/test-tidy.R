test_that("Basic case works for `tidy()`", {
  withr::local_seed(123)
  conmat <- matrix(rnorm(100), ncol = 10)
  behav <- rnorm(10)
  result <- fit(cpm_spec(), conmat, behav, return_edges = "sum")
  tidy(result, component = "performance") |>
    expect_snapshot_value(style = "json2")
  tidy(result, component = "edges") |>
    expect_snapshot_value(style = "json2")
})

test_that("Support pass arguments of `summary()`", {
  withr::local_seed(123)
  conmat <- matrix(rnorm(100), ncol = 10)
  behav <- rnorm(10)
  result <- fit(cpm_spec(), conmat, behav, return_edges = "sum")
  expect_equal(
    tidy(result, component = "performance", method = "spearman")$method,
    "spearman"
  )
  expect_named(tidy(result, component = "edges"), c(
    "covariates",
    "thresh_method",
    "thresh_level",
    "return_edges",
    "na_action",
    "bias_correct",
    "pos",
    "neg"
  ))
})

test_that("Warning when edges are not stored", {
  withr::local_seed(123)
  conmat <- matrix(rnorm(100), ncol = 10)
  behav <- rnorm(10)
  result <- fit(cpm_spec(), conmat, behav, return_edges = "none")
  expect_warning(tidy(result, component = "edges"), "No edges stored")
})
