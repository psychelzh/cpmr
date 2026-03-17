test_that("compute_single_predictions keeps row order and columns", {
  real <- c(a = 1, b = 2, c = 3)
  pred <- cbind(
    combined = c(1.1, 2.1, 3.1),
    positive = c(1.2, 2.2, 3.2),
    negative = c(0.9, 1.9, 2.9)
  )

  collected <- compute_single_predictions(real, pred)

  expect_named(collected, c("row", "real", "combined", "positive", "negative"))
  expect_equal(rownames(collected), names(real))
  expect_equal(collected$row, 1:3)
  expect_equal(collected$real, unname(real))
})

test_that("compute_fold_predictions annotates rows with fold ids", {
  real <- c(1, 2, 3, 4)
  pred <- cbind(
    combined = c(1.1, 2.1, 3.1, 4.1),
    positive = c(1.2, 2.2, 3.2, 4.2),
    negative = c(0.9, 1.9, 2.9, 3.9)
  )
  folds <- list(c(2L, 4L), c(1L, 3L))

  collected <- compute_fold_predictions(real, pred, folds)

  expect_named(collected, c("row", "fold", "real", "combined", "positive", "negative"))
  expect_equal(collected$row, 1:4)
  expect_equal(collected$fold, c(2L, 1L, 2L, 1L))
  expect_equal(collected$real, real)
})

