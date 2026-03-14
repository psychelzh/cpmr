test_that("Default threshold method works", {
  withr::local_seed(123)
  conmat <- matrix(rnorm(100), ncol = 10)
  behav <- rnorm(10)
  result <- fit(cpm_spec(), conmat, behav)
  expect_s3_class(result, "cpm")
  expect_snapshot_value(result$pred, style = "json2")
  expect_snapshot_value(result$edges, style = "json2")
  expect_snapshot_value(result$params, style = "json2")
  expect_snapshot(result)
})

test_that("`fit()` is single-fit", {
  withr::local_seed(123)
  conmat <- matrix(rnorm(100), ncol = 10)
  behav <- rnorm(10)
  result <- fit(cpm_spec(), conmat, behav)
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
  result <- fit(cpm_spec(thresh_method = "sparsity"), conmat, behav)
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
  result <- fit(cpm_spec(thresh_level = 0.1), conmat, behav)
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
  result <- fit(cpm_spec(), conmat, behav, covariates = covariates)
  expect_s3_class(result, "cpm")
  expect_equal(sum(complete.cases(result$pred)), 10)
  expect_length(result$real, 10)
  expect_true(isTRUE(result$params$covariates))
})

test_that("fit with covariates uses in-sample residualized target scale", {
  withr::local_seed(42)
  n <- 30
  p <- 20
  cov <- rnorm(n)
  conmat <- matrix(rnorm(n * p) + rep(cov, p), nrow = n, ncol = p)
  behav <- cov * 2 + rnorm(n)
  covariates <- matrix(cov, ncol = 1)

  result <- fit(cpm_spec(), conmat, behav, covariates = covariates)

  behav_resid <- drop(regress_covariates(behav, covariates))

  expect_equal(result$real, behav_resid)
  expect_true(isTRUE(all(stats::complete.cases(result$pred))))
})

test_that("Keep names of behavior", {
  withr::local_seed(123)
  conmat <- matrix(rnorm(100), ncol = 10)
  behav <- rnorm(10)
  names(behav) <- LETTERS[1:10]
  result <- fit(cpm_spec(), conmat, behav)
  expect_named(result$real, LETTERS[1:10])
  expect_identical(rownames(result$pred), LETTERS[1:10])
})

test_that("`return_edges` argument works", {
  withr::local_seed(123)
  conmat <- matrix(rnorm(100), ncol = 10)
  behav <- rnorm(10)
  result <- fit(cpm_spec(), conmat, behav, return_edges = FALSE)
  expect_null(result$edges)
  expect_null(collect_edges(result))
  expect_snapshot(result)
  result <- fit(cpm_spec(), conmat, behav, return_edges = TRUE)
  expect_snapshot_value(result$edges, style = "json2")
  expect_equal(collect_edges(result), result$edges)
  expect_snapshot(result)
})

test_that("`fit()` validates logical `return_edges`", {
  withr::local_seed(123)
  conmat <- matrix(rnorm(100), ncol = 10)
  behav <- rnorm(10)

  expect_error(
    fit(cpm_spec(), conmat, behav, return_edges = "sum"),
    "`return_edges` must be either TRUE or FALSE for `fit()`.",
    fixed = TRUE
  )
})

test_that("Support row/column matrix input of `behav` and `covariates`", {
  withr::local_seed(123)
  conmat <- matrix(rnorm(100), ncol = 10)
  behav <- rnorm(10)
  result <- fit(cpm_spec(), conmat, behav)
  key_fields <- c("real", "pred", "edges")
  expect_identical(
    fit(cpm_spec(), conmat, matrix(behav, ncol = 1))[key_fields],
    result[key_fields]
  )
  expect_identical(
    fit(cpm_spec(), conmat, matrix(behav, nrow = 1))[key_fields],
    result[key_fields]
  )
  covariates <- matrix(rnorm(10), ncol = 1)
  result <- fit(cpm_spec(), conmat, behav, covariates = covariates)
  expect_identical(
    fit(cpm_spec(), conmat, behav, covariates = drop(covariates))[key_fields],
    result[key_fields]
  )
})

test_that("Throw informative error if data checking not pass", {
  withr::local_seed(123)
  conmat <- matrix(rnorm(100), ncol = 10)
  expect_error(
    fit(cpm_spec(), conmat, matrix(rnorm(20), ncol = 2)),
    "Behavior data must be a numeric vector."
  )
  expect_error(
    fit(cpm_spec(), conmat, rnorm(20)),
    "Case numbers of `conmat` and `behav` must match."
  )
  expect_error(
    fit(
      cpm_spec(),
      conmat,
      rnorm(10),
      covariates = matrix(rnorm(20), ncol = 1)
    ),
    "Case numbers of `covariates` and `behav` must match."
  )
})

test_that("`na_action` argument works", {
  withr::local_seed(123)
  conmat <- matrix(rnorm(100), ncol = 10)
  behav <- rnorm(10)
  behav[1] <- NA
  expect_error(
    fit(cpm_spec(), conmat, behav),
    "Missing values found in `behav`"
  )
  result <- fit(cpm_spec(), conmat, behav, na_action = "exclude")
  expect_equal(sum(complete.cases(result$real)), 9)
  expect_equal(sum(complete.cases(result$pred)), 9)
  expect_snapshot(result)
  covariates <- matrix(rnorm(10), ncol = 1)
  covariates[2, 1] <- NA
  result <- fit(
    cpm_spec(),
    conmat,
    behav,
    covariates = covariates,
    na_action = "exclude"
  )
  expect_equal(sum(complete.cases(result$real)), sum(complete.cases(behav)))
  expect_equal(sum(complete.cases(result$pred)), 8)
  expect_snapshot(result)
  conmat[1, 1] <- NA
  result <- fit(
    cpm_spec(),
    conmat,
    behav,
    covariates = covariates,
    na_action = "exclude"
  )
  expect_equal(sum(complete.cases(result$real)), sum(complete.cases(behav)))
  expect_equal(sum(complete.cases(result$pred)), 8)
  expect_snapshot(result)
})

test_that("fit errors clearly when exclude leaves too few complete cases", {
  withr::local_seed(123)
  conmat <- matrix(rnorm(100), ncol = 10)
  behav <- rep(NA_real_, 10)

  expect_error(
    fit(cpm_spec(), conmat, behav, na_action = "exclude"),
    "No complete-case observations available for fitting.",
    fixed = TRUE
  )

  behav[1:2] <- rnorm(2)
  expect_error(
    fit(cpm_spec(), conmat, behav, na_action = "exclude"),
    "At least 3 complete-case observations are required for fitting.",
    fixed = TRUE
  )
})

test_that("Folds cover complete cases exactly when excluding missing data", {
  withr::local_seed(123)
  conmat <- matrix(rnorm(120), ncol = 12)
  behav <- rnorm(10)
  covariates <- matrix(rnorm(20), ncol = 2)

  behav[2] <- NA
  conmat[4, 3] <- NA
  covariates[6, 1] <- NA

  result <- fit(
    cpm_spec(),
    conmat,
    behav,
    covariates = covariates,
    na_action = "exclude"
  )
  include_cases <- intersect(
    intersect(which(complete.cases(conmat)), which(complete.cases(behav))),
    which(complete.cases(covariates))
  )

  expect_equal(sum(complete.cases(result$pred)), length(include_cases))
})

test_that("resolve_kfolds uses complete-case count when NULL", {
  expect_identical(resolve_kfolds(NULL, include_cases = c(2L, 4L, 7L)), 3L)
  expect_identical(resolve_kfolds(5L, include_cases = c(2L, 4L, 7L)), 5L)
})
