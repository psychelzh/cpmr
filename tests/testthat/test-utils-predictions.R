test_that("assemble_single_predictions keeps row order and columns", {
  observed <- c(a = 1, b = 2, c = 3, d = 4)
  split_fit <- list(
    observed = c(1, 3, 4),
    predictions = cbind(
      joint = c(1.1, 3.1, 4.1),
      positive = c(1.2, 3.2, 4.2),
      negative = c(0.9, 2.9, 3.9)
    )
  )

  collected <- assemble_single_predictions(
    observed = observed,
    include_cases = c(1L, 3L, 4L),
    split_fit = split_fit
  )

  expect_named(
    collected,
    c("row", "observed", "joint", "positive", "negative")
  )
  expect_equal(rownames(collected), names(observed))
  expect_equal(collected$row, 1:4)
  expect_equal(collected$observed, c(1, 2, 3, 4))
  expect_true(is.na(collected$joint[2]))
})

test_that("assemble_fold_predictions annotates rows with fold ids", {
  observed <- c(1, 2, 3, 4)
  split_results <- list(
    list(
      observed = c(2, 4),
      predictions = cbind(
        joint = c(2.1, 4.1),
        positive = c(2.2, 4.2),
        negative = c(1.9, 3.9)
      )
    ),
    list(
      observed = c(1, 3),
      predictions = cbind(
        joint = c(1.1, 3.1),
        positive = c(1.2, 3.2),
        negative = c(0.9, 2.9)
      )
    )
  )
  folds <- list(c(2L, 4L), c(1L, 3L))

  collected <- assemble_fold_predictions(observed, folds, split_results)

  expect_named(
    collected,
    c("row", "fold", "observed", "joint", "positive", "negative")
  )
  expect_equal(collected$row, 1:4)
  expect_equal(collected$fold, c(2L, 1L, 2L, 1L))
  expect_equal(collected$observed, observed)
})
