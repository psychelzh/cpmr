test_that("Default threshold method works", {
  withr::local_seed(123)
  conmat <- matrix(rnorm(100), ncol = 10)
  behav <- rnorm(10)
  result <- cpm(conmat, behav)
  expect_s3_class(result, "cpm")
  expect_snapshot_value(result$pred, style = "json2")
  expect_snapshot_value(result$edges, style = "json2")
  expect_snapshot(result)
})

test_that("`kfolds` works", {
  withr::local_seed(123)
  conmat <- matrix(rnorm(100), ncol = 10)
  behav <- rnorm(10)
  result <- cpm(conmat, behav, kfolds = 5)
  expect_s3_class(result, "cpm")
  expect_snapshot_value(result$pred, style = "json2")
  expect_snapshot_value(result$edges, style = "json2")
  expect_snapshot(result)
})

test_that("Alternative threshold method works", {
  withr::local_seed(123)
  conmat <- matrix(rnorm(100), ncol = 10)
  behav <- rnorm(10)
  result <- cpm(conmat, behav, thresh_method = "sparsity")
  expect_s3_class(result, "cpm")
  expect_snapshot_value(result$pred, style = "json2")
  expect_snapshot_value(result$edges, style = "json2")
})

test_that("Different threshold levels works", {
  withr::local_seed(123)
  conmat <- matrix(rnorm(100), ncol = 10)
  behav <- rnorm(10)
  result <- cpm(conmat, behav, thresh_level = 0.1)
  expect_s3_class(result, "cpm")
  expect_snapshot_value(result$pred, style = "json2")
  expect_snapshot_value(result$edges, style = "json2")
})

test_that("Works with confounds", {
  withr::local_seed(123)
  conmat <- matrix(rnorm(100), ncol = 10)
  behav <- rnorm(10)
  confounds <- matrix(rnorm(10), ncol = 1)
  result <- cpm(conmat, behav, confounds = confounds)
  expect_s3_class(result, "cpm")
  expect_snapshot_value(result$pred, style = "json2", tolerance = 1e-6)
  expect_snapshot_value(result$edges, style = "json2")
})

test_that("Keep names of behavior", {
  withr::local_seed(123)
  conmat <- matrix(rnorm(100), ncol = 10)
  behav <- rnorm(10)
  names(behav) <- LETTERS[1:10]
  result <- cpm(conmat, behav)
  expect_named(result$real, LETTERS[1:10])
  expect_identical(rownames(result$pred), LETTERS[1:10])
})

test_that("`return_edges` argument works", {
  withr::local_seed(123)
  conmat <- matrix(rnorm(100), ncol = 10)
  behav <- rnorm(10)
  result <- cpm(conmat, behav, return_edges = "all")
  expect_snapshot_value(result$edges, style = "json2")
  result <- cpm(conmat, behav, return_edges = "sum")
  expect_snapshot_value(result$edges, style = "json2")
})
