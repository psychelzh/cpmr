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

test_that("summary.cpm_resamples returns pooled and mean-fold summaries", {
  withr::local_seed(321)
  conmat <- matrix(rnorm(240), nrow = 20)
  behav <- rnorm(20)

  resampled <- fit_resamples(
    cpm_spec(),
    conmat = conmat,
    behav = behav,
    kfolds = 5,
    return_edges = "sum"
  )
  summary_result <- summary(resampled, edge_level = 0.5)

  expect_s3_class(summary_result, "cpm_resamples_summary")
  expect_identical(
    rownames(summary_result$performance),
    c("pooled", "fold_mean")
  )
  expect_identical(
    colnames(summary_result$performance),
    c("both", "pos", "neg")
  )
  expect_type(summary_result$edges, "logical")
  expect_equal(dim(summary_result$edges), c(ncol(conmat), 2))
  expect_identical(summary_result$params$return_edges, "sum")
  expect_identical(summary_result$params$n_folds, 5L)
  expect_identical(summary_result$params$edge_level, 0.5)

  out <- capture.output(print(summary_result))
  expect_true(any(grepl("CPM resamples summary", out, fixed = TRUE)))
  expect_true(any(grepl("Pooled", out, fixed = TRUE)))
  expect_true(any(grepl("Mean fold", out, fixed = TRUE)))
  expect_true(any(grepl(
    "Selected edges (edge_level = 0.500)",
    out,
    fixed = TRUE
  )))
})

test_that("summary.cpm_resamples edge_level controls binarized edge masks", {
  withr::local_seed(654)
  conmat <- matrix(rnorm(240), nrow = 20)
  behav <- rnorm(20)

  resampled <- fit_resamples(
    cpm_spec(),
    conmat = conmat,
    behav = behav,
    kfolds = 5,
    return_edges = "sum"
  )

  low_level <- summary(resampled, edge_level = 0)
  high_level <- summary(resampled, edge_level = 1)

  expect_true(isTRUE(all(low_level$edges)))
  expect_true(
    sum(high_level$edges, na.rm = TRUE) <= sum(low_level$edges, na.rm = TRUE)
  )
})

test_that("summary.cpm_resamples supports method with no stored edges", {
  withr::local_seed(456)
  conmat <- matrix(rnorm(120), nrow = 12)
  behav <- rnorm(12)

  resampled <- fit_resamples(
    cpm_spec(),
    conmat = conmat,
    behav = behav,
    kfolds = 4,
    return_edges = "none"
  )
  summary_result <- summary(
    resampled,
    method = "spearman",
    edge_level = 0.2
  )

  expect_identical(summary_result$params$method, "spearman")
  expect_identical(summary_result$params$edge_level, 0.2)
  expect_null(summary_result$edges)
})

test_that("print.cpm_resamples_summary prints NA instead of NaN", {
  x <- structure(
    list(
      folds = list(1:2, 3:4),
      predictions = data.frame(
        row = 1:4,
        fold = c(1L, 1L, 2L, 2L),
        real = c(1, 2, 3, 4),
        both = c(10, 10, 10, 10),
        pos = c(20, 20, 20, 20),
        neg = c(30, 30, 30, 30)
      ),
      edges = NULL,
      params = list(return_edges = "none")
    ),
    class = "cpm_resamples"
  )

  summary_result <- summary(x)
  out <- capture.output(print(summary_result))

  expect_false(any(grepl("NaN", out, fixed = TRUE)))
  expect_true(any(grepl("Combined: NA", out, fixed = TRUE)))
})
