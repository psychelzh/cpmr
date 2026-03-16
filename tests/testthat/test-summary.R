test_that("Works for basic summary", {
  withr::local_seed(123)
  conmat <- matrix(rnorm(10000), nrow = 10)
  behav <- rnorm(10)
  summary_result <- summary(fit(cpm_spec(), conmat, behav))
  expect_s3_class(summary_result, "cpm_summary")
  expect_snapshot(summary_result)
  summary_result <- summary(
    fit(cpm_spec(), conmat, behav, return_edges = "none")
  )
  expect_s3_class(summary_result, "cpm_summary")
  expect_snapshot(summary_result)
  summary_result <- summary(
    fit(cpm_spec(), conmat, behav, return_edges = "all")
  )
  expect_s3_class(summary_result, "cpm_summary")
  expect_snapshot(summary_result)
})

test_that("summary.cpm collapses singleton edge arrays to a 2D matrix", {
  withr::local_seed(123)
  conmat <- matrix(rnorm(10000), nrow = 10)
  behav <- rnorm(10)
  summary_result <- summary(
    fit(cpm_spec(), conmat, behav, return_edges = "all")
  )

  expect_equal(dim(summary_result$edges), c(1000, 2))
  expect_type(summary_result$edges, "logical")
})

test_that("summary.cpm returns NA when fewer than two valid pairs", {
  sparse_object <- structure(
    list(
      real = c(NA_real_, 2, NA_real_),
      pred = matrix(
        c(1, 2, 3, 1, 2, 3, 1, 2, 3),
        ncol = 3,
        dimnames = list(NULL, c("both", "pos", "neg"))
      ),
      edges = NULL,
      params = list()
    ),
    class = "cpm"
  )

  summary_result <- summary(sparse_object)

  expect_true(all(is.na(summary_result$performance)))
  expect_null(summary_result$edges)
})

test_that("print.cpm_summary reports NA edge rates when stored edges are all missing", {
  summary_result <- structure(
    list(
      performance = matrix(
        c(0.1, 0.2, 0.3),
        nrow = 1,
        dimnames = list(NULL, c("both", "pos", "neg"))
      ),
      edges = matrix(
        c(NA, NA),
        ncol = 2,
        dimnames = list(NULL, c("pos", "neg"))
      ),
      params = list(method = "pearson")
    ),
    class = "cpm_summary"
  )

  output <- capture.output(print(summary_result))

  expect_true(any(grepl("Positive: NA", output, fixed = TRUE)))
  expect_true(any(grepl("Negative: NA", output, fixed = TRUE)))
})
