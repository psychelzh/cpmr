test_that("format helpers produce stable display strings", {
  expect_identical(format_cor(0.1234), "0.123")
  expect_identical(format_cor(NA_real_), "NA")
  expect_identical(format_rate(0.125), "12.50%")
  expect_identical(format_rate(NA_real_), "NA")
})

test_that("prediction labels use full names", {
  expect_identical(prediction_label("both"), "Combined")
  expect_identical(prediction_label("pos"), "Positive")
  expect_identical(prediction_label("neg"), "Negative")
})
