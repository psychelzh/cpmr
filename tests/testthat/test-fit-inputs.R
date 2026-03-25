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

test_that("resolve_include_cases returns intersection in exclude mode", {
  conmat <- matrix(rnorm(30), ncol = 3)
  behav <- rnorm(10)
  covariates <- matrix(rnorm(10), ncol = 1)

  conmat[1, 1] <- NA
  behav[2] <- NA
  covariates[3, 1] <- NA

  include_cases <- resolve_include_cases(
    conmat,
    behav,
    covariates,
    na_action = "exclude"
  )

  expect_identical(include_cases, 4:10)
})

test_that("check_case_names_match errors when names do not match", {
  x <- matrix(1:10, nrow = 2, dimnames = list(c("a", "b"), NULL))
  y <- stats::setNames(1:2, c("a", "c"))

  expect_error(check_case_names_match(x, y), "must match")
  expect_silent(check_case_names_match(x, 1:2))
})
