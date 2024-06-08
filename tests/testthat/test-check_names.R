test_that("Error if names exist but do not match", {
  x <- matrix(1:10, nrow = 2, dimnames = list(c("a", "b"), NULL))
  y <- stats::setNames(1:2, c("a", "c"))
  expect_error(check_names(x, y), "must match")
  y <- 1:2
  expect_silent(check_names(x, y))
})
