test_that("Error if names exist but do not match", {
  x <- matrix(1:10, nrow = 2, dimnames = list(c("a", "b"), NULL))
  y <- stats::setNames(1:2, c("a", "c"))
  expect_error(check_names(x, y), "must match")
  y <- 1:2
  expect_silent(check_names(x, y))
})

test_that("regress_covariates returns linear-model residuals", {
  withr::local_seed(123)
  covariates <- matrix(rnorm(50), ncol = 1)
  resp <- 2 + 3 * covariates[, 1] + rnorm(50, sd = 0.1)

  residuals_expected <- stats::.lm.fit(cbind(1, covariates), resp)$residuals
  residuals_actual <- regress_covariates(resp, covariates)

  expect_equal(residuals_actual, residuals_expected)
  expect_lt(abs(stats::cor(residuals_actual, covariates[, 1])), 1e-10)
  expect_equal(regress_confounds(resp, covariates), residuals_expected)
})
