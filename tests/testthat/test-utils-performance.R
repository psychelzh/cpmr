test_that("safe_cor returns NA for degenerate vectors", {
  expect_true(is.na(safe_cor(c(1, 1, 1), c(1, 2, 3))))
  expect_true(is.na(safe_cor(c(1), c(1))))
})

test_that("safe_cor returns correlation for valid vectors", {
  expect_equal(safe_cor(c(1, 2, 3), c(2, 4, 6)), 1)
})

test_that("safe_rmse and safe_mae handle missing and valid pairs", {
  expect_true(is.na(safe_rmse(c(NA_real_), c(1))))
  expect_true(is.na(safe_mae(c(NA_real_), c(1))))
  expect_equal(safe_rmse(c(1, 2), c(1, 4)), sqrt(2))
  expect_equal(safe_mae(c(1, 2), c(1, 4)), 1)
})

test_that("safe_mean handles empty and all-NA inputs", {
  expect_true(is.na(safe_mean(numeric())))
  expect_true(is.na(safe_mean(c(NA_real_, NA_real_))))
  expect_equal(safe_mean(c(1, NA_real_, 3)), 2)
})

test_that("safe_std_error handles short and valid inputs", {
  expect_true(is.na(safe_std_error(c(1))))
  expect_true(is.na(safe_std_error(c(NA_real_, NA_real_))))
  expect_equal(safe_std_error(c(1, 3)), 1)
})
