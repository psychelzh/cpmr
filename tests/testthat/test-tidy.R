test_that("Basic case works for `tidy()`", {
  withr::local_seed(123)
  conmat <- matrix(rnorm(100), ncol = 10)
  behav <- rnorm(10)
  result <- fit(cpm_spec(), conmat, behav)
  tidy(result, component = "performance") |>
    expect_snapshot_value(style = "json2")
  tidy(result, component = "edges") |>
    expect_snapshot_value(style = "json2")
})

test_that("Support pass arguments of `summary()`", {
  withr::local_seed(123)
  conmat <- matrix(rnorm(100), ncol = 10)
  behav <- rnorm(10)
  result <- fit(cpm_spec(), conmat, behav)
  expect_equal(
    tidy(result, component = "performance", method = "spearman")$method,
    "spearman"
  )
  expect_named(
    tidy(result, component = "edges"),
    c(
      "covariates",
      "na_action",
      "selection_type",
      "selection_method",
      "selection_criterion",
      "selection_level",
      "construction_type",
      "construction_polarity",
      "edge_weighting",
      "weighting_scale",
      "standardize_edges",
      "model_type",
      "positive",
      "negative"
    )
  )
})
