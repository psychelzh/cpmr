test_that("Default threshold method works", {
  withr::local_seed(123)
  conmat <- matrix(rnorm(100), ncol = 10)
  behav <- rnorm(10)
  result <- cpm(conmat, behav)
  expect_s3_class(result, "cpm")
  expect_snapshot_value(result$pred, style = "json2")
  expect_snapshot_value(result$edges, style = "json2")
  expect_snapshot_value(result$params, style = "json2")
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
  expect_snapshot_value(result$params, style = "json2")
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
  expect_snapshot_value(result$params, style = "json2")
  expect_snapshot(result)
})

test_that("Different threshold levels works", {
  withr::local_seed(123)
  conmat <- matrix(rnorm(100), ncol = 10)
  behav <- rnorm(10)
  result <- cpm(conmat, behav, thresh_level = 0.1)
  expect_s3_class(result, "cpm")
  expect_snapshot_value(result$pred, style = "json2")
  expect_snapshot_value(result$edges, style = "json2")
  expect_snapshot_value(result$params, style = "json2")
  expect_snapshot(result)
})

test_that("Works with covariates", {
  withr::local_seed(123)
  conmat <- matrix(rnorm(100), ncol = 10)
  behav <- rnorm(10)
  covariates <- matrix(rnorm(10), ncol = 1)
  result <- cpm(conmat, behav, covariates = covariates)
  expect_s3_class(result, "cpm")
  expect_equal(sum(complete.cases(result$pred)), 10)
  expect_length(result$real, 10)
  expect_true(isTRUE(result$params$covariates))

  expect_warning(
    legacy <- cpm(conmat, behav, confounds = covariates),
    "deprecated"
  )
  expect_equal(legacy$pred, result$pred, tolerance = 1e-6)
  expect_equal(legacy$real, result$real, tolerance = 1e-6)
  expect_error(
    cpm(conmat, behav, covariates = covariates, confounds = covariates),
    "Please provide only one"
  )
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
  result <- cpm(conmat, behav, return_edges = "none")
  expect_null(result$edges)
  expect_snapshot(result)
  result <- cpm(conmat, behav, return_edges = "all")
  expect_snapshot_value(result$edges, style = "json2")
  expect_snapshot(result)
})

test_that("Support row/column matrix input of `behav` and `covariates`", {
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
  covariates <- matrix(rnorm(10), ncol = 1)
  result <- cpm(conmat, behav, covariates = covariates)
  expect_identical(
    cpm(conmat, behav, covariates = drop(covariates))[key_fields],
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
    cpm(conmat, rnorm(10), covariates = matrix(rnorm(20), ncol = 1)),
    "Case numbers of `covariates` and `behav` must match."
  )
})

test_that("`na_action` argument works", {
  withr::local_seed(123)
  conmat <- matrix(rnorm(100), ncol = 10)
  behav <- rnorm(10)
  behav[1] <- NA
  expect_error(cpm(conmat, behav), "Missing values found in `behav`")
  result <- cpm(conmat, behav, na_action = "exclude")
  expect_equal(sum(complete.cases(result$real)), 9)
  expect_equal(sum(complete.cases(result$pred)), 9)
  expect_snapshot(result)
  covariates <- matrix(rnorm(10), ncol = 1)
  covariates[2, 1] <- NA
  result <- cpm(conmat, behav, covariates = covariates, na_action = "exclude")
  expect_equal(sum(complete.cases(result$real)), sum(complete.cases(behav)))
  expect_equal(sum(complete.cases(result$pred)), 8)
  expect_snapshot(result)
  conmat[1, 1] <- NA
  result <- cpm(conmat, behav, covariates = covariates, na_action = "exclude")
  expect_equal(sum(complete.cases(result$real)), sum(complete.cases(behav)))
  expect_equal(sum(complete.cases(result$pred)), 8)
  expect_snapshot(result)
})

test_that("Folds cover complete cases exactly when excluding missing data", {
  withr::local_seed(123)
  conmat <- matrix(rnorm(120), ncol = 12)
  behav <- rnorm(10)
  covariates <- matrix(rnorm(20), ncol = 2)

  # Remove different rows from each input to validate intersection behavior.
  behav[2] <- NA
  conmat[4, 3] <- NA
  covariates[6, 1] <- NA

  result <- cpm(conmat, behav, covariates = covariates, na_action = "exclude")
  include_cases <- intersect(
    intersect(which(complete.cases(conmat)), which(complete.cases(behav))),
    which(complete.cases(covariates))
  )

  expect_setequal(unlist(result$folds), include_cases)
  expect_equal(sum(complete.cases(result$pred)), length(include_cases))
})
