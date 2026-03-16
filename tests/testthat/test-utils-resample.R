test_that("compute_fold_metrics summarizes each assessment fold", {
  predictions <- data.frame(
    row = 1:4,
    fold = c(1L, 1L, 2L, 2L),
    real = c(1, 2, 3, 4),
    both = c(1, 2, 3, 4),
    pos = c(1, 2, 3, 4),
    neg = c(4, 3, 2, 1)
  )
  folds <- list(1:2, 3:4)

  metrics <- compute_fold_metrics(predictions, folds)

  expect_named(metrics, c("fold", "n_assess", "both", "pos", "neg"))
  expect_equal(metrics$fold, 1:2)
  expect_equal(metrics$n_assess, c(2, 2))
  expect_true(all(is.finite(metrics$both[1:2])))
  expect_true(all(is.finite(metrics$pos[1:2])))
})

test_that("summarize_resample_edges handles sum, all, and none storage", {
  stored_sum <- matrix(
    c(2, 0, 1, 2),
    ncol = 2,
    dimnames = list(NULL, c("pos", "neg"))
  )
  stored_all <- array(
    c(
      TRUE,
      FALSE,
      FALSE,
      TRUE,
      FALSE,
      FALSE,
      TRUE,
      TRUE
    ),
    dim = c(2, 2, 2),
    dimnames = list(NULL, c("pos", "neg"), NULL)
  )

  expect_equal(summarize_resample_edges(stored_sum, "sum", 2L), stored_sum / 2)
  expect_equal(
    summarize_resample_edges(stored_all, "all", 2L),
    apply(stored_all, c(1, 2), mean)
  )
  expect_null(summarize_resample_edges(NULL, "none", 2L))
})
