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

test_that("summary.cpm falls back to folds when params$kfolds is missing", {
  legacy_object <- structure(
    list(
      real = c(1, 2, 3, 4),
      pred = matrix(
        c(1, 2, 3, 4, 1, 2, 3, 4, 4, 3, 2, 1),
        ncol = 3,
        dimnames = list(NULL, c("both", "pos", "neg"))
      ),
      edges = matrix(
        c(2, 0, 1, 2),
        ncol = 2,
        dimnames = list(NULL, c("pos", "neg"))
      ),
      folds = list(1:2, 3:4),
      params = list()
    ),
    class = "cpm"
  )

  summary_result <- summary(legacy_object, edge_level = 0.5)

  expect_s3_class(summary_result, "cpm_summary")
  expect_identical(summary_result$edges, legacy_object$edges > 1)
})
