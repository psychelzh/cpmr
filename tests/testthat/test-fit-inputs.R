test_that("normalize_inputs validates and normalizes data", {
  conmat <- matrix(rnorm(20), ncol = 2)
  behav <- matrix(rnorm(10), ncol = 1)
  covariates <- rnorm(10)

  normalized <- normalize_inputs(conmat, behav, covariates)

  expect_true(is.vector(normalized$behav))
  expect_equal(length(normalized$behav), nrow(conmat))
  expect_true(is.matrix(normalized$covariates))
  expect_equal(nrow(normalized$covariates), nrow(conmat))
})

test_that("complete_case_rows returns intersection in exclude mode", {
  conmat <- matrix(rnorm(30), ncol = 3)
  behav <- rnorm(10)
  covariates <- matrix(rnorm(10), ncol = 1)

  conmat[1, 1] <- NA
  behav[2] <- NA
  covariates[3, 1] <- NA

  include_cases <- complete_case_rows(
    conmat,
    behav,
    covariates,
    na_action = "exclude"
  )

  expect_identical(include_cases, 4:10)
})

test_that("complete_case_rows fails clearly on missing required inputs", {
  conmat <- matrix(rnorm(30), ncol = 3)
  behav <- rnorm(10)
  covariates <- matrix(rnorm(10), ncol = 1)

  conmat[1, 1] <- NA
  expect_error(
    complete_case_rows(conmat, behav, covariates, na_action = "fail"),
    "Missing values found in `conmat`",
    fixed = TRUE
  )

  conmat[1, 1] <- 0
  covariates[1, 1] <- NA
  expect_error(
    complete_case_rows(conmat, behav, covariates, na_action = "fail"),
    "Missing values found in `covariates`",
    fixed = TRUE
  )
})

test_that("normalize_inputs checks case-name alignment directly", {
  conmat <- matrix(1:10, nrow = 2, dimnames = list(c("a", "b"), NULL))
  behav <- stats::setNames(1:2, c("a", "c"))
  covariates <- matrix(1:4, nrow = 2, dimnames = list(c("a", "b"), NULL))

  expect_error(
    normalize_inputs(conmat, behav, covariates),
    "Case names of `conmat` must match",
    fixed = TRUE
  )

  rownames(conmat) <- names(behav)
  rownames(covariates) <- c("a", "b")
  expect_error(
    normalize_inputs(conmat, behav, covariates),
    "Case names of `covariates` must match",
    fixed = TRUE
  )

  expect_silent(normalize_inputs(conmat, unname(behav), covariates))
})
