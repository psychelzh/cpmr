test_that("Works for basic summary", {
  withr::local_seed(123)
  conmat <- matrix(rnorm(10000), nrow = 10)
  behav <- rnorm(10)
  summary_result <- summary(fit(cpm_spec(), conmat, behav))
  expect_s3_class(summary_result, "cpm_summary")
  expect_snapshot(summary_result)
  summary_result <- summary(
    fit(cpm_spec(), conmat, behav, return_edges = FALSE)
  )
  expect_s3_class(summary_result, "cpm_summary")
  expect_snapshot(summary_result)
  summary_result <- summary(
    fit(cpm_spec(), conmat, behav, return_edges = TRUE)
  )
  expect_s3_class(summary_result, "cpm_summary")
  expect_snapshot(summary_result)
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

test_that("summary.cpm rejects removed edge_level argument", {
  withr::local_seed(123)
  conmat <- matrix(rnorm(100), ncol = 10)
  behav <- rnorm(10)

  expect_error(
    summary(fit(cpm_spec(), conmat, behav), edge_level = 0.5),
    "`edge_level` is no longer supported for `summary.cpm()`.",
    fixed = TRUE
  )
})
