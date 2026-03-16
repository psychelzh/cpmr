test_that("critical_r matches the t-statistic conversion", {
  df <- 20 - 2
  ct <- stats::qt(0.05 / 2, df, lower.tail = FALSE)

  expect_equal(
    critical_r(20, 0.05),
    sqrt((ct^2) / ((ct^2) + df))
  )
})

test_that("select_edges returns a logical pos/neg mask", {
  withr::local_seed(1)
  conmat <- matrix(rnorm(60), nrow = 10, ncol = 6)
  behav <- rnorm(10)

  edges <- select_edges(conmat, behav, "alpha", 0.1)

  expect_equal(dim(edges), c(ncol(conmat), 2))
  expect_type(edges, "logical")
  expect_identical(colnames(edges), c("pos", "neg"))
})
