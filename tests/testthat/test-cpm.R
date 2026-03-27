prediction_matrix <- function(x) {
  as.matrix(x$predictions[, prediction_columns(x$predictions), drop = FALSE])
}

prediction_complete_cases <- function(x) {
  stats::complete.cases(
    x$predictions[, prediction_columns(x$predictions), drop = FALSE]
  )
}

single_fit_result <- function(
  spec = cpm_spec(),
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

test_that("Default threshold method works", {
  withr::local_seed(123)
  conmat <- matrix(rnorm(100), ncol = 10)
  behav <- rnorm(10)
  result <- single_fit_result(spec(), conmat, behav)
  expect_s3_class(result, "cpm")
  expect_length(result$folds, 1)
  expect_identical(result$folds[[1]], seq_along(behav))
  expect_named(
    result$predictions,
    c("row", "observed", "joint", "positive", "negative")
  )
  expect_equal(dim(result$edges), c(ncol(conmat), 2))
  expect_identical(colnames(result$edges), c("positive", "negative"))
  expect_identical(names(result$settings), c("covariates", "na_action"))
  expect_false(result$settings$covariates)
  expect_identical(result$settings$na_action, "fail")
})

test_that("Alternative threshold method works", {
  withr::local_seed(123)
  conmat <- matrix(rnorm(100), ncol = 10)
  behav <- rnorm(10)
  result <- single_fit_result(
    cpm_spec(selection = cpm_selection_cor(criterion = "proportion")),
    conmat = conmat,
    behav = behav
  )
  expect_s3_class(result, "cpm")
  expect_equal(sum(prediction_complete_cases(result)), 10)
  expect_named(
    result$predictions,
    c("row", "observed", "joint", "positive", "negative")
  )
  expect_equal(dim(result$edges), c(ncol(conmat), 2))
  expect_identical(colnames(result$edges), c("positive", "negative"))
  expect_true(any(result$edges))
  expect_identical(result$spec$selection$type, "cor")
  expect_identical(result$spec$selection$criterion, "proportion")
  expect_identical(result$spec$selection$level, 0.01)
  expect_identical(result$spec$construction$type, "summary")
  expect_identical(result$spec$construction$sign_mode, "separate")
  expect_identical(result$spec$construction$weight_scale, 0)
  expect_identical(result$spec$model$type, "lm")
})

test_that("Different threshold levels works", {
  withr::local_seed(123)
  conmat <- matrix(rnorm(100), ncol = 10)
  behav <- rnorm(10)
  result <- single_fit_result(
    cpm_spec(selection = cpm_selection_cor(level = 0.1)),
    conmat = conmat,
    behav = behav
  )
  expect_s3_class(result, "cpm")
  expect_named(
    result$predictions,
    c("row", "observed", "joint", "positive", "negative")
  )
  expect_equal(dim(result$edges), c(ncol(conmat), 2))
  expect_identical(colnames(result$edges), c("positive", "negative"))
  expect_identical(result$spec$selection$level, 0.1)
  expect_identical(result$settings$na_action, "fail")
})

test_that("Works with covariates", {
  withr::local_seed(123)
  conmat <- matrix(rnorm(100), ncol = 10)
  behav <- rnorm(10)
  covariates <- matrix(rnorm(10), ncol = 1)
  result <- single_fit_result(
    cpm_spec(),
    conmat = conmat,
    behav = behav,
    covariates = covariates
  )
  expect_s3_class(result, "cpm")
  expect_equal(sum(prediction_complete_cases(result)), 10)
  expect_equal(nrow(result$predictions), 10)
  expect_true(isTRUE(result$settings$covariates))
})

test_that("fit with covariates uses in-sample residualized target scale", {
  withr::local_seed(42)
  n <- 30
  p <- 20
  cov <- rnorm(n)
  conmat <- matrix(rnorm(n * p) + rep(cov, p), nrow = n, ncol = p)
  behav <- cov * 2 + rnorm(n)
  covariates <- matrix(cov, ncol = 1)

  result <- single_fit_result(
    cpm_spec(),
    conmat = conmat,
    behav = behav,
    covariates = covariates
  )

  behav_resid <- stats::.lm.fit(cbind(1, covariates), behav)$residuals

  expect_equal(result$predictions$observed, behav_resid)
  expect_true(isTRUE(all(prediction_complete_cases(result))))
})

test_that("Keep names of behavior", {
  withr::local_seed(123)
  conmat <- matrix(rnorm(100), ncol = 10)
  behav <- rnorm(10)
  names(behav) <- LETTERS[1:10]
  result <- single_fit_result(cpm_spec(), conmat = conmat, behav = behav)
  expect_identical(rownames(result$predictions), LETTERS[1:10])
  expect_equal(result$predictions$row, seq_along(behav))
})

test_that("single-fit CPM always stores selected edges", {
  withr::local_seed(123)
  conmat <- matrix(rnorm(100), ncol = 10)
  behav <- rnorm(10)
  result <- single_fit_result(cpm_spec(), conmat = conmat, behav = behav)
  expect_equal(dim(result$edges), c(10, 2))
  expect_false("return_edges" %in% names(result$settings))
})

test_that("print.cpm reports stored edge count", {
  result <- single_fit_result(
    spec(),
    conmat = matrix(rnorm(100), ncol = 10),
    behav = rnorm(10)
  )
  output <- capture.output(print(result))

  expect_true(any(grepl("Number of folds: 1", output, fixed = TRUE)))
  expect_true(any(grepl("Number of observations: 10", output, fixed = TRUE)))
  expect_true(any(grepl("Edge storage:\\s+stored", output)))
  expect_true(any(grepl("Covariates:\\s+none", output)))
  expect_true(any(grepl("Edge standardization:\\s+none", output)))
})

test_that("Support row/column matrix input of `behav` and `covariates`", {
  withr::local_seed(123)
  conmat <- matrix(rnorm(100), ncol = 10)
  behav <- rnorm(10)
  result <- single_fit_result(cpm_spec(), conmat = conmat, behav = behav)
  key_fields <- c("predictions", "edges")
  expect_identical(
    single_fit_result(
      cpm_spec(),
      conmat = conmat,
      behav = matrix(behav, ncol = 1)
    )[key_fields],
    result[key_fields]
  )
  expect_identical(
    single_fit_result(
      cpm_spec(),
      conmat = conmat,
      behav = matrix(behav, nrow = 1)
    )[key_fields],
    result[key_fields]
  )
  covariates <- matrix(rnorm(10), ncol = 1)
  result <- single_fit_result(
    cpm_spec(),
    conmat = conmat,
    behav = behav,
    covariates = covariates
  )
  expect_identical(
    single_fit_result(
      cpm_spec(),
      conmat = conmat,
      behav = behav,
      covariates = drop(covariates)
    )[key_fields],
    result[key_fields]
  )
})

test_that("Throw informative error if data checking not pass", {
  withr::local_seed(123)
  conmat <- matrix(rnorm(100), ncol = 10)
  expect_error(
    single_fit_result(
      cpm_spec(),
      conmat = conmat,
      behav = matrix(rnorm(20), ncol = 2)
    ),
    "Behavior data must be a numeric vector."
  )
  expect_error(
    single_fit_result(cpm_spec(), conmat = conmat, behav = rnorm(20)),
    "The number of observations in `conmat` and `behav` must match."
  )
  expect_error(
    single_fit_result(
      cpm_spec(),
      conmat = conmat,
      behav = rnorm(10),
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
    single_fit_result(cpm_spec(), conmat = conmat, behav = behav),
    "Missing values found in `behav`"
  )
  result <- single_fit_result(
    cpm_spec(),
    conmat = conmat,
    behav = behav,
    na_action = "exclude"
  )
  expect_equal(sum(complete.cases(result$predictions$observed)), 9)
  expect_equal(sum(prediction_complete_cases(result)), 9)
  expect_length(result$folds, 1)
  expect_identical(result$settings$na_action, "exclude")
  covariates <- matrix(rnorm(10), ncol = 1)
  covariates[2, 1] <- NA
  result <- single_fit_result(
    cpm_spec(),
    conmat = conmat,
    behav = behav,
    covariates = covariates,
    na_action = "exclude"
  )
  expect_equal(
    sum(complete.cases(result$predictions$observed)),
    sum(complete.cases(behav))
  )
  expect_equal(sum(prediction_complete_cases(result)), 8)
  expect_length(result$folds, 1)
  expect_identical(result$settings$na_action, "exclude")
  conmat[1, 1] <- NA
  result <- single_fit_result(
    cpm_spec(),
    conmat = conmat,
    behav = behav,
    covariates = covariates,
    na_action = "exclude"
  )
  expect_equal(
    sum(complete.cases(result$predictions$observed)),
    sum(complete.cases(behav))
  )
  expect_equal(sum(prediction_complete_cases(result)), 8)
  expect_length(result$folds, 1)
  expect_identical(result$settings$na_action, "exclude")
})

test_that("fit excludes incomplete rows consistently when excluding missing data", {
  withr::local_seed(123)
  conmat <- matrix(rnorm(120), ncol = 12)
  behav <- rnorm(10)
  covariates <- matrix(rnorm(20), ncol = 2)

  behav[2] <- NA
  conmat[4, 3] <- NA
  covariates[6, 1] <- NA

  result <- single_fit_result(
    cpm_spec(),
    conmat = conmat,
    behav = behav,
    covariates = covariates,
    na_action = "exclude"
  )
  include_cases <- intersect(
    intersect(which(complete.cases(conmat)), which(complete.cases(behav))),
    which(complete.cases(covariates))
  )

  expect_length(result$folds, 1)
  expect_identical(result$folds[[1]], include_cases)
  expect_equal(sum(prediction_complete_cases(result)), length(include_cases))
  expect_true(isTRUE(all(prediction_complete_cases(result)[include_cases])))
  expect_true(isTRUE(all(is.na(prediction_matrix(result)[-include_cases, ]))))
})
