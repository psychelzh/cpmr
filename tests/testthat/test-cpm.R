prediction_matrix <- function(x) {
  as.matrix(x$predictions[, prediction_columns(x$predictions), drop = FALSE])
}

prediction_complete_cases <- function(x) {
  stats::complete.cases(
    x$predictions[, prediction_columns(x$predictions), drop = FALSE]
  )
}

test_that("Default threshold method works", {
  withr::local_seed(123)
  conmat <- matrix(rnorm(100), ncol = 10)
  behav <- rnorm(10)
  result <- fit(cpm_spec(), conmat, behav)
  expect_s3_class(result, "cpm")
  expect_false("folds" %in% names(result))
  expect_snapshot_value(prediction_matrix(result), style = "json2")
  expect_snapshot_value(result$edges, style = "json2")
  expect_snapshot_value(result$params, style = "json2")
  expect_snapshot(result)
})

test_that("new_cpm builds single-fit CPM objects", {
  withr::local_seed(99)
  conmat <- matrix(rnorm(120), nrow = 10, ncol = 12)
  behav <- rnorm(10)
  spec <- cpm_spec(threshold_method = "alpha", threshold_level = 0.05)
  call <- quote(fit(object = spec, conmat = conmat, behav = behav))

  fit_result <- run_single_fit(
    object = spec,
    conmat = conmat,
    behav = behav,
    covariates = NULL,
    na_action = "fail",
    call = call
  )

  cpm_object <- new_cpm(
    call = call,
    spec = spec,
    params = fit_result$params,
    predictions = fit_result$predictions,
    edges = fit_result$edges,
    model = fit_result$model
  )

  expect_s3_class(cpm_object, "cpm")
  expect_identical(cpm_object$call, call)
  expect_false("folds" %in% names(cpm_object))
})

test_that("Alternative threshold method works", {
  withr::local_seed(123)
  conmat <- matrix(rnorm(100), ncol = 10)
  behav <- rnorm(10)
  result <- fit(cpm_spec(threshold_method = "sparsity"), conmat, behav)
  expect_s3_class(result, "cpm")
  expect_snapshot_value(prediction_matrix(result), style = "json2")
  expect_snapshot_value(result$edges, style = "json2")
  expect_snapshot_value(result$params, style = "json2")
  expect_snapshot(result)
})

test_that("Different threshold levels works", {
  withr::local_seed(123)
  conmat <- matrix(rnorm(100), ncol = 10)
  behav <- rnorm(10)
  result <- fit(cpm_spec(threshold_level = 0.1), conmat, behav)
  expect_s3_class(result, "cpm")
  expect_snapshot_value(prediction_matrix(result), style = "json2")
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
  expect_equal(sum(prediction_complete_cases(result)), 10)
  expect_equal(nrow(result$predictions), 10)
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

  behav_resid <- stats::.lm.fit(cbind(1, covariates), behav)$residuals

  expect_equal(result$predictions$real, behav_resid)
  expect_true(isTRUE(all(prediction_complete_cases(result))))
})

test_that("Keep names of behavior", {
  withr::local_seed(123)
  conmat <- matrix(rnorm(100), ncol = 10)
  behav <- rnorm(10)
  names(behav) <- LETTERS[1:10]
  result <- fit(cpm_spec(), conmat, behav)
  expect_identical(rownames(result$predictions), LETTERS[1:10])
  expect_equal(result$predictions$row, seq_along(behav))
})

test_that("single-fit CPM always stores selected edges", {
  withr::local_seed(123)
  conmat <- matrix(rnorm(100), ncol = 10)
  behav <- rnorm(10)
  result <- fit(cpm_spec(), conmat, behav)
  expect_equal(dim(result$edges), c(10, 2))
  expect_false("return_edges" %in% names(result$params))
})

test_that("print.cpm reports stored edge count", {
  result <- fit(cpm_spec(), matrix(rnorm(100), ncol = 10), rnorm(10))
  output <- capture.output(print(result))

  expect_true(any(grepl("Candidate edges: 10", output, fixed = TRUE)))
  expect_true(any(grepl("Covariates:       none", output, fixed = TRUE)))
  expect_true(any(grepl("Bias correction:  yes", output, fixed = TRUE)))
})

test_that("Support row/column matrix input of `behav` and `covariates`", {
  withr::local_seed(123)
  conmat <- matrix(rnorm(100), ncol = 10)
  behav <- rnorm(10)
  result <- fit(cpm_spec(), conmat, behav)
  key_fields <- c("predictions", "edges")
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
    "The number of observations in `conmat` and `behav` must match."
  )
  expect_error(
    fit(
      cpm_spec(),
      conmat,
      rnorm(10),
      covariates = matrix(rnorm(20), ncol = 1)
    ),
    "The number of observations in `covariates` and `behav` must match."
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
  expect_equal(sum(complete.cases(result$predictions$real)), 9)
  expect_equal(sum(prediction_complete_cases(result)), 9)
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
  expect_equal(
    sum(complete.cases(result$predictions$real)),
    sum(complete.cases(behav))
  )
  expect_equal(sum(prediction_complete_cases(result)), 8)
  expect_snapshot(result)
  conmat[1, 1] <- NA
  result <- fit(
    cpm_spec(),
    conmat,
    behav,
    covariates = covariates,
    na_action = "exclude"
  )
  expect_equal(
    sum(complete.cases(result$predictions$real)),
    sum(complete.cases(behav))
  )
  expect_equal(sum(prediction_complete_cases(result)), 8)
  expect_snapshot(result)
})

test_that("fit excludes incomplete rows consistently when excluding missing data", {
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

  expect_false("folds" %in% names(result))
  expect_equal(sum(prediction_complete_cases(result)), length(include_cases))
  expect_true(isTRUE(all(prediction_complete_cases(result)[include_cases])))
  expect_true(isTRUE(all(is.na(prediction_matrix(result)[-include_cases, ]))))
})

