test_that("Basic case works for `tidy()`", {
  withr::local_seed(123)
  conmat <- matrix(rnorm(100), ncol = 10)
  behav <- rnorm(10)
  result <- fit(cpm_spec(), conmat, behav, return_edges = TRUE)
  tidy(result, component = "performance") |>
    expect_snapshot_value(style = "json2")
  tidy(result, component = "edges") |>
    expect_snapshot_value(style = "json2")
})

test_that("Support pass arguments of `summary()`", {
  withr::local_seed(123)
  conmat <- matrix(rnorm(100), ncol = 10)
  behav <- rnorm(10)
  result <- fit(cpm_spec(), conmat, behav, return_edges = TRUE)
  expect_equal(
    tidy(result, component = "performance", method = "spearman")$method,
    "spearman"
  )
  expect_equal(
    tidy(result, component = "edges", edge_level = 0.8)$level,
    0.8
  )
})

test_that("Warning when edges are not stored", {
  withr::local_seed(123)
  conmat <- matrix(rnorm(100), ncol = 10)
  behav <- rnorm(10)
  result <- fit(cpm_spec(), conmat, behav, return_edges = FALSE)
  expect_warning(tidy(result, component = "edges"), "No edges stored")
})
