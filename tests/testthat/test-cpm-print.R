test_that("format helpers produce stable display strings", {
  expect_identical(format_cor(0.1234), "0.123")
  expect_identical(format_cor(NA_real_), "NA")
  expect_identical(format_rate(0.125), "12.50%")
  expect_identical(format_rate(NA_real_), "NA")
  expect_identical(format_threshold_level(0.005), "0.005")
  expect_identical(format_threshold_level(0.2), "0.2")
})

test_that("prediction labels use full names", {
  expect_identical(prediction_label("joint"), "Joint")
  expect_identical(prediction_label("positive"), "Positive")
  expect_identical(prediction_label("negative"), "Negative")
  expect_identical(prediction_label("net"), "Net")
})
