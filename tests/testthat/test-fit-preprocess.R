test_that("regress_covariates_by_train returns training residuals", {
  withr::local_seed(123)
  covariates <- matrix(rnorm(50), ncol = 1)
  resp <- 2 + 3 * covariates[, 1] + rnorm(50, sd = 0.1)

  residuals_expected <- stats::.lm.fit(cbind(1, covariates), resp)$residuals
  residuals_actual <- regress_covariates_by_train(
    resp_train = resp,
    cov_train = covariates
  )$train

  expect_equal(residuals_actual, residuals_expected)
  expect_lt(abs(stats::cor(residuals_actual, covariates[, 1])), 1e-10)
})

test_that("regress_covariates_by_train uses training fit for held-out rows", {
  withr::local_seed(654)
  conmat <- matrix(rnorm(60), nrow = 10, ncol = 6)
  behav <- rnorm(10)
  covariates <- matrix(rnorm(20), nrow = 10, ncol = 2)
  rows_train <- 1:7
  rows_test <- 8:10

  conmat_resid <- regress_covariates_by_train(
    resp_train = conmat[rows_train, , drop = FALSE],
    resp_test = conmat[rows_test, , drop = FALSE],
    cov_train = covariates[rows_train, , drop = FALSE],
    cov_test = covariates[rows_test, , drop = FALSE]
  )
  behav_resid <- regress_covariates_by_train(
    resp_train = behav[rows_train],
    resp_test = behav[rows_test],
    cov_train = covariates[rows_train, , drop = FALSE],
    cov_test = covariates[rows_test, , drop = FALSE]
  )

  expect_equal(
    conmat_resid$train,
    regress_covariates_by_train(
      resp_train = conmat[rows_train, , drop = FALSE],
      cov_train = covariates[rows_train, , drop = FALSE]
    )$train
  )
  expect_equal(
    drop(behav_resid$train),
    drop(regress_covariates_by_train(
      resp_train = behav[rows_train],
      cov_train = covariates[rows_train, , drop = FALSE]
    )$train)
  )
  expect_equal(dim(conmat_resid$test), c(length(rows_test), ncol(conmat)))
  expect_length(drop(behav_resid$test), length(rows_test))
})

test_that("regress_covariates_by_train returns train and test residuals", {
  withr::local_seed(321)
  train <- rnorm(8)
  test <- rnorm(4)
  cov_train <- matrix(rnorm(16), ncol = 2)
  cov_test <- matrix(rnorm(8), ncol = 2)

  resid <- regress_covariates_by_train(
    resp_train = train,
    cov_train = cov_train,
    resp_test = test,
    cov_test = cov_test
  )

  expect_named(resid, c("train", "test"))
  expect_length(resid$train, length(train))
  expect_length(drop(resid$test), length(test))
})
