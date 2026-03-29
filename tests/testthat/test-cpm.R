prediction_matrix <- function(x) {
  as.matrix(x$predictions[, prediction_columns(x$predictions), drop = FALSE])
}

prediction_complete_cases <- function(x) {
  stats::complete.cases(
    x$predictions[, prediction_columns(x$predictions), drop = FALSE]
  )
}

example_cpm_result <- function(
  spec_obj = spec(),
  conmat,
  behav,
  covariates = NULL,
  resamples = 5,
  return_edges = "sum",
  na_action = "fail"
) {
  cpm(
    conmat = conmat,
    behav = behav,
    spec = spec_obj,
    covariates = covariates,
    resamples = resamples,
    return_edges = return_edges,
    na_action = na_action
  )
}

test_that("cpm returns a fold-based cpm object", {
  withr::local_seed(123)
  conmat <- matrix(rnorm(100), ncol = 10)
  behav <- rnorm(10)

  result <- example_cpm_result(conmat = conmat, behav = behav)

  expect_s3_class(result, "cpm")
  expect_length(result$folds, 5)
  expect_named(
    result$predictions,
    c("row", "fold", "observed", "joint", "positive", "negative")
  )
  expect_equal(dim(result$edges), c(ncol(conmat), 2))
  expect_identical(result$settings$return_edges, "sum")
  expect_identical(result$settings$na_action, "fail")
  expect_false(result$settings$covariates)
})

test_that("cpm works with alternative staged settings", {
  withr::local_seed(123)
  conmat <- matrix(rnorm(100), ncol = 10)
  behav <- rnorm(10)

  result <- example_cpm_result(
    spec_obj = spec(
      selection = cpm_selection_cor(criterion = "proportion"),
      construction = cpm_construction_summary(sign_mode = "net")
    ),
    conmat = conmat,
    behav = behav
  )

  expect_named(result$predictions, c("row", "fold", "observed", "net"))
  expect_true(any(prediction_complete_cases(result)))
  expect_identical(result$spec$selection$criterion, "proportion")
  expect_identical(result$spec$construction$sign_mode, "net")
})

test_that("cpm works with covariates", {
  withr::local_seed(123)
  conmat <- matrix(rnorm(100), ncol = 10)
  behav <- rnorm(10)
  covariates <- matrix(rnorm(10), ncol = 1)

  result <- example_cpm_result(
    conmat = conmat,
    behav = behav,
    covariates = covariates
  )

  expect_true(isTRUE(result$settings$covariates))
  expect_equal(nrow(result$predictions), 10)
  expect_true(any(prediction_complete_cases(result)))
})

test_that("cpm keeps behavior names in prediction row names", {
  withr::local_seed(123)
  conmat <- matrix(rnorm(100), ncol = 10)
  behav <- rnorm(10)
  names(behav) <- LETTERS[1:10]

  result <- example_cpm_result(
    conmat = conmat,
    behav = behav
  )

  expect_identical(rownames(result$predictions), LETTERS[1:10])
  expect_equal(result$predictions$row, seq_along(behav))
})

test_that("cpm accepts matrix behavior and vector covariates", {
  withr::local_seed(123)
  conmat <- matrix(rnorm(100), ncol = 10)
  behav <- rnorm(10)
  covariates <- matrix(rnorm(10), ncol = 1)
  folds <- list(1:2, 3:4, 5:6, 7:8, 9:10)

  vector_result <- example_cpm_result(
    conmat = conmat,
    behav = behav,
    covariates = covariates,
    resamples = folds
  )

  expect_identical(
    example_cpm_result(
      conmat = conmat,
      behav = matrix(behav, ncol = 1),
      covariates = covariates,
      resamples = folds
    )[c("predictions", "edges", "folds")],
    vector_result[c("predictions", "edges", "folds")]
  )
  expect_identical(
    example_cpm_result(
      conmat = conmat,
      behav = matrix(behav, nrow = 1),
      covariates = covariates,
      resamples = folds
    )[c("predictions", "edges", "folds")],
    vector_result[c("predictions", "edges", "folds")]
  )
  expect_identical(
    example_cpm_result(
      conmat = conmat,
      behav = behav,
      covariates = drop(covariates),
      resamples = folds
    )[c("predictions", "edges", "folds")],
    vector_result[c("predictions", "edges", "folds")]
  )
})

test_that("cpm throws informative data-checking errors", {
  withr::local_seed(123)
  conmat <- matrix(rnorm(100), ncol = 10)

  expect_error(
    example_cpm_result(
      conmat = conmat,
      behav = matrix(rnorm(20), ncol = 2)
    ),
    "Behavior data must be a numeric vector."
  )
  expect_error(
    example_cpm_result(
      conmat = conmat,
      behav = rnorm(20)
    ),
    "The number of observations in `conmat` and `behav` must match."
  )
  expect_error(
    example_cpm_result(
      conmat = conmat,
      behav = rnorm(10),
      covariates = matrix(rnorm(20), ncol = 1)
    ),
    "The number of observations in `covariates` and `behav` must match."
  )
})

test_that("cpm supports missing-data exclusion while keeping row layout", {
  withr::local_seed(123)
  conmat <- matrix(rnorm(120), ncol = 12)
  behav <- rnorm(10)
  covariates <- matrix(rnorm(20), ncol = 2)

  behav[2] <- NA
  conmat[4, 3] <- NA
  covariates[6, 1] <- NA

  include_cases <- setdiff(seq_len(10), c(2L, 4L, 6L))
  resamples <- list(include_cases[1:3], include_cases[4:5], include_cases[6:7])

  result <- example_cpm_result(
    conmat = conmat,
    behav = behav,
    covariates = covariates,
    resamples = resamples,
    na_action = "exclude"
  )

  expect_identical(result$folds, resamples)
  expect_equal(
    sort(result$predictions$row[!is.na(result$predictions$fold)]),
    include_cases
  )
  expect_true(all(is.na(prediction_matrix(result)[-include_cases, ])))
})
