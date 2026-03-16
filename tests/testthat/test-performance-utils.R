test_that("safe_cor returns NA for degenerate vectors", {
  expect_true(is.na(safe_cor(c(1, 1, 1), c(1, 2, 3))))
  expect_true(is.na(safe_cor(c(1), c(1))))
})

test_that("safe_cor returns correlation for valid vectors", {
  expect_equal(safe_cor(c(1, 2, 3), c(2, 4, 6)), 1)
})

test_that("safe_mean handles empty and all-NA inputs", {
  expect_true(is.na(safe_mean(numeric())))
  expect_true(is.na(safe_mean(c(NA_real_, NA_real_))))
  expect_equal(safe_mean(c(1, NA_real_, 3)), 2)
})

test_that("format helpers produce stable display strings", {
  expect_identical(format_cor(0.1234), "0.123")
  expect_identical(format_cor(NA_real_), "NA")
  expect_identical(format_rate(0.125), "12.50%")
  expect_identical(format_rate(NA_real_), "NA")
})
