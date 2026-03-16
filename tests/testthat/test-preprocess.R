test_that("regress_covariates returns linear-model residuals", {
  withr::local_seed(123)
  covariates <- matrix(rnorm(50), ncol = 1)
  resp <- 2 + 3 * covariates[, 1] + rnorm(50, sd = 0.1)

  residuals_expected <- stats::.lm.fit(cbind(1, covariates), resp)$residuals
  residuals_actual <- regress_covariates(resp, covariates)

  expect_equal(residuals_actual, residuals_expected)
  expect_lt(abs(stats::cor(residuals_actual, covariates[, 1])), 1e-10)
})

test_that("prepare_training_data residualizes training rows with covariates", {
  withr::local_seed(456)
  n <- 20
  p <- 8
  conmat <- matrix(rnorm(n * p), nrow = n, ncol = p)
  behav <- rnorm(n)
  covariates <- matrix(rnorm(n * 2), nrow = n, ncol = 2)
  rows_train <- 1:15

  training <- prepare_training_data(
    conmat = conmat,
    behav = behav,
    covariates = covariates,
    rows_train = rows_train
  )
  expected_cov_train <- covariates[rows_train, , drop = FALSE]
  expected_train_conmat <- regress_covariates(
    conmat[rows_train, , drop = FALSE],
    expected_cov_train
  )
  expected_train_behav <- drop(regress_covariates(
    behav[rows_train],
    expected_cov_train
  ))

  expect_equal(training$conmat, expected_train_conmat)
  expect_equal(training$behav, expected_train_behav)
  expect_equal(training$covariates, expected_cov_train)
})

test_that("prepare_assessment_data keeps covariate handling train-only", {
  withr::local_seed(654)
  conmat <- matrix(rnorm(60), nrow = 10, ncol = 6)
  behav <- rnorm(10)
  covariates <- matrix(rnorm(20), nrow = 10, ncol = 2)
  rows_train <- 1:7
  rows_test <- 8:10

  assessment <- prepare_assessment_data(
    conmat = conmat,
    behav = behav,
    covariates = covariates,
    rows_train = rows_train,
    rows_test = rows_test
  )

  expected <- regress_covariates_by_train(
    resp_train = conmat[rows_train, , drop = FALSE],
    resp_test = conmat[rows_test, , drop = FALSE],
    cov_train = covariates[rows_train, , drop = FALSE],
    cov_test = covariates[rows_test, , drop = FALSE]
  )
  expected_behav <- regress_covariates_by_train(
    resp_train = behav[rows_train],
    resp_test = behav[rows_test],
    cov_train = covariates[rows_train, , drop = FALSE],
    cov_test = covariates[rows_test, , drop = FALSE]
  )

  expect_equal(assessment$conmat, expected$test)
  expect_equal(assessment$behav, drop(expected_behav$test))
})

test_that("prepare_assessment_data infers training covariates when omitted", {
  withr::local_seed(654)
  conmat <- matrix(rnorm(60), nrow = 10, ncol = 6)
  behav <- rnorm(10)
  covariates <- matrix(rnorm(20), nrow = 10, ncol = 2)
  rows_train <- 1:7
  rows_test <- 8:10

  assessment <- prepare_assessment_data(
    conmat = conmat,
    behav = behav,
    covariates = covariates,
    rows_train = rows_train,
    rows_test = rows_test
  )

  assessment_explicit <- prepare_assessment_data(
    conmat = conmat,
    behav = behav,
    covariates = covariates,
    rows_train = rows_train,
    rows_test = rows_test,
    covariates_train = covariates[rows_train, , drop = FALSE]
  )

  expect_equal(assessment, assessment_explicit)
})
