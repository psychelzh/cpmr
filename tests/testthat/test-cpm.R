test_that("Default threshold method works", {
  withr::local_seed(123)
  conmat <- matrix(rnorm(100), ncol = 10)
  behav <- rnorm(10)
  result <- cpm(conmat, behav)
  expect_s3_class(result, "cpm")
  expect_snapshot_value(result$pred, style = "json2")
  expect_null(result$edges)
  expect_snapshot(result)
})

test_that("`kfolds` works", {
  withr::local_seed(123)
  conmat <- matrix(rnorm(100), ncol = 10)
  behav <- rnorm(10)
  result <- cpm(conmat, behav, kfolds = 5)
  expect_s3_class(result, "cpm")
  expect_snapshot_value(result$pred, style = "json2")
  expect_null(result$edges)
  expect_snapshot(result)
})

test_that("Alternative threshold method works", {
  withr::local_seed(123)
  conmat <- matrix(rnorm(100), ncol = 10)
  behav <- rnorm(10)
  result <- cpm(conmat, behav, thresh_method = "sparsity")
  expect_s3_class(result, "cpm")
  expect_snapshot_value(result$pred, style = "json2")
  expect_null(result$edges)
  expect_snapshot(result)
})

test_that("Different threshold levels works", {
  withr::local_seed(123)
  conmat <- matrix(rnorm(100), ncol = 10)
  behav <- rnorm(10)
  result <- cpm(conmat, behav, thresh_level = 0.1)
  expect_s3_class(result, "cpm")
  expect_snapshot_value(result$pred, style = "json2")
  expect_null(result$edges)
  expect_snapshot(result)
})

test_that("Works with confounds", {
  withr::local_seed(123)
  conmat <- matrix(rnorm(100), ncol = 10)
  behav <- rnorm(10)
  confounds <- matrix(rnorm(10), ncol = 1)
  result <- cpm(conmat, behav, confounds = confounds)
  expect_s3_class(result, "cpm")
  expect_snapshot_value(result$pred, style = "json2", tolerance = 1e-6)
  expect_null(result$edges)
  expect_snapshot(result)
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

test_that("Support row/column matrix input of `behav` and `confounds`", {
  withr::local_seed(123)
  conmat <- matrix(rnorm(100), ncol = 10)
  behav <- rnorm(10)
  result <- cpm(conmat, behav)
  key_fields <- c("real", "pred", "edges")
  expect_identical(
    cpm(conmat, matrix(behav, ncol = 1))[key_fields],
    result[key_fields]
  )
  expect_identical(
    cpm(conmat, matrix(behav, nrow = 1))[key_fields],
    result[key_fields]
  )
  confounds <- matrix(rnorm(10), ncol = 1)
  result <- cpm(conmat, behav, confounds = confounds)
  expect_identical(
    cpm(conmat, behav, confounds = drop(confounds))[key_fields],
    result[key_fields]
  )
})

test_that("Throw informative error if data checking not pass", {
  withr::local_seed(123)
  conmat <- matrix(rnorm(100), ncol = 10)
  expect_error(
    cpm(conmat, matrix(rnorm(20), ncol = 2)),
    "Behavior data must be a numeric vector."
  )
  expect_error(
    cpm(conmat, rnorm(20)),
    "Case numbers of `conmat` and `behav` must match."
  )
  expect_error(
    cpm(conmat, rnorm(10), confounds = matrix(rnorm(20), ncol = 1)),
    "Case numbers of `confounds` and `behav` must match."
  )
})
