test_that("core preprocessing keeps covariate regression train-only", {
  problem <- simulate_cpm_problem(n = 24, p = 10, seed = 13)
  covariates <- matrix(rnorm(24 * 2), nrow = 24, ncol = 2)
  rows_train <- 1:18
  rows_test <- 19:24

  training <- core_prepare_training_data(
    conmat = problem$x,
    behav = problem$y,
    covariates = covariates,
    rows_train = rows_train
  )
  assessment <- core_prepare_assessment_data(
    conmat = problem$x,
    behav = problem$y,
    covariates = covariates,
    rows_train = rows_train,
    rows_test = rows_test,
    covariates_train = training$covariates
  )

  expect_equal(dim(training$conmat), c(length(rows_train), ncol(problem$x)))
  expect_length(training$behav, length(rows_train))
  expect_equal(dim(assessment$conmat), c(length(rows_test), ncol(problem$x)))
  expect_length(assessment$behav, length(rows_test))
})

test_that("assessment preprocessing derives train covariates when omitted", {
  problem <- simulate_cpm_problem(n = 24, p = 8, seed = 51)
  covariates <- matrix(rnorm(24 * 2), nrow = 24, ncol = 2)

  assessment <- core_prepare_assessment_data(
    conmat = problem$x,
    behav = problem$y,
    covariates = covariates,
    rows_train = 1:18,
    rows_test = 19:24
  )

  expect_equal(dim(assessment$conmat), c(6, 8))
  expect_length(assessment$behav, 6)
})
