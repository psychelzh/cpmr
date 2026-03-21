test_that("compute_single_predictions keeps row order and columns", {
  observed <- c(a = 1, b = 2, c = 3)
  pred <- cbind(
    joint = c(1.1, 2.1, 3.1),
    positive = c(1.2, 2.2, 3.2),
    negative = c(0.9, 1.9, 2.9)
  )

  collected <- compute_single_predictions(observed, pred)

  expect_named(
    collected,
    c("row", "observed", "joint", "positive", "negative")
  )
  expect_equal(rownames(collected), names(observed))
  expect_equal(collected$row, 1:3)
  expect_equal(collected$observed, unname(observed))
})

test_that("compute_fold_predictions annotates rows with fold ids", {
  observed <- c(1, 2, 3, 4)
  pred <- cbind(
    joint = c(1.1, 2.1, 3.1, 4.1),
    positive = c(1.2, 2.2, 3.2, 4.2),
    negative = c(0.9, 1.9, 2.9, 3.9)
  )
  folds <- list(c(2L, 4L), c(1L, 3L))

  collected <- compute_fold_predictions(observed, pred, folds)

  expect_named(
    collected,
    c("row", "fold", "observed", "joint", "positive", "negative")
  )
  expect_equal(collected$row, 1:4)
  expect_equal(collected$fold, c(2L, 1L, 2L, 1L))
  expect_equal(collected$observed, observed)
})
