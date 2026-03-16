test_that("Works for basic summary", {
  withr::local_seed(123)
  conmat <- matrix(rnorm(10000), nrow = 10)
  behav <- rnorm(10)
  summary_result <- summary(fit(cpm_spec(), conmat, behav))
  expect_s3_class(summary_result, "cpm_summary")
  expect_snapshot(summary_result)
})

test_that("summary.cpm returns NA when fewer than two valid pairs", {
  sparse_object <- structure(
    list(
      predictions = data.frame(
        row = 1:3,
        real = c(NA_real_, 2, NA_real_),
        both = c(1, 2, 3),
        pos = c(1, 2, 3),
        neg = c(1, 2, 3)
      ),
      edges = matrix(
        c(TRUE, FALSE, FALSE, TRUE),
        ncol = 2,
        dimnames = list(NULL, c("pos", "neg"))
      ),
      params = list()
    ),
    class = "cpm"
  )

  summary_result <- summary(sparse_object)

  expect_true(all(is.na(summary_result$performance)))
  expect_identical(summary_result$edges, sparse_object$edges)
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

test_that("summary.cpm_resamples aggregates fold metrics and edge rates", {
  summary_result <- summary(
    new_cpm_resamples(
      call = quote(fit_resamples(spec, conmat = conmat, behav = behav)),
      spec = cpm_spec(),
      params = list(kfolds = 2L, return_edges = "sum"),
      predictions = data.frame(
        row = 1:4,
        fold = c(1L, 1L, 2L, 2L),
        real = c(1, 2, 3, 4),
        both = c(1, 2, 3, 4),
        pos = c(1, 2, 3, 4),
        neg = c(4, 3, 2, 1)
      ),
      edges = matrix(
        c(2, 0, 1, 2),
        ncol = 2,
        dimnames = list(NULL, c("pos", "neg"))
      ),
      folds = list(1:2, 3:4),
      metrics = data.frame(
        fold = 1:2,
        n_assess = c(2, 2),
        both = c(0.5, 0.25),
        pos = c(0.2, 0.4),
        neg = c(-0.1, -0.2)
      )
    )
  )

  expect_s3_class(summary_result, "cpm_resamples_summary")
  expect_identical(
    colnames(summary_result$performance),
    c("both", "pos", "neg")
  )
  expect_identical(rownames(summary_result$performance), c("mean", "std_error"))
  expect_equal(summary_result$performance["mean", "both"], 0.375)
  expect_equal(summary_result$edges[, "pos"], c(1, 0))
  expect_equal(summary_result$edges[, "neg"], c(0.5, 1))
})

test_that("summary.cpm_resamples returns NULL edges when resamples did not store them", {
  summary_result <- summary(
    new_cpm_resamples(
      call = quote(fit_resamples(spec, conmat = conmat, behav = behav)),
      spec = cpm_spec(),
      params = list(kfolds = 2L, return_edges = "none"),
      predictions = data.frame(
        row = 1:4,
        fold = c(1L, 1L, 2L, 2L),
        real = c(1, 2, 3, 4),
        both = c(1, 2, 3, 4),
        pos = c(1, 2, 3, 4),
        neg = c(4, 3, 2, 1)
      ),
      edges = NULL,
      folds = list(1:2, 3:4),
      metrics = data.frame(
        fold = 1:2,
        n_assess = c(2, 2),
        both = c(NA_real_, NA_real_),
        pos = c(NA_real_, NA_real_),
        neg = c(NA_real_, NA_real_)
      )
    )
  )

  expect_null(summary_result$edges)
  expect_true(all(is.na(summary_result$performance)))
})

test_that("summary.cpm_resamples averages fold-wise edges when all edges are stored", {
  stored_edges <- array(
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

  summary_result <- summary(
    new_cpm_resamples(
      call = quote(fit_resamples(spec, conmat = conmat, behav = behav)),
      spec = cpm_spec(),
      params = list(kfolds = 2L, return_edges = "all"),
      predictions = data.frame(
        row = 1:4,
        fold = c(1L, 1L, 2L, 2L),
        real = c(1, 2, 3, 4),
        both = c(1, 2, 3, 4),
        pos = c(1, 2, 3, 4),
        neg = c(4, 3, 2, 1)
      ),
      edges = stored_edges,
      folds = list(1:2, 3:4),
      metrics = data.frame(
        fold = 1:2,
        n_assess = c(2, 2),
        both = c(0.5, 0.25),
        pos = c(0.2, 0.4),
        neg = c(-0.1, -0.2)
      )
    )
  )

  expect_equal(summary_result$edges, apply(stored_edges, c(1, 2), mean))
})

test_that("print.cpm_resamples_summary reports fold count and rates", {
  summary_result <- structure(
    list(
      performance = rbind(
        mean = c(both = 0.4, pos = 0.2, neg = -0.1),
        std_error = c(both = 0.05, pos = 0.02, neg = 0.01)
      ),
      edges = matrix(
        c(0.5, 0.25),
        ncol = 2,
        dimnames = list(NULL, c("pos", "neg"))
      ),
      params = list(kfolds = 5L, return_edges = "sum")
    ),
    class = "cpm_resamples_summary"
  )

  output <- capture.output(print(summary_result))

  expect_true(any(grepl("Number of folds: 5", output, fixed = TRUE)))
  expect_true(any(grepl("Combined: 0.400 (SE 0.050)", output, fixed = TRUE)))
  expect_true(any(grepl("Positive: 50.00%", output, fixed = TRUE)))
  expect_true(any(grepl("Negative: 25.00%", output, fixed = TRUE)))
})

test_that("print.cpm_resamples_summary omits edge block when edges are not stored", {
  summary_result <- structure(
    list(
      performance = rbind(
        mean = c(both = 0.4, pos = 0.2, neg = -0.1),
        std_error = c(both = 0.05, pos = 0.02, neg = 0.01)
      ),
      edges = NULL,
      params = list(kfolds = 5L, return_edges = "none")
    ),
    class = "cpm_resamples_summary"
  )

  output <- capture.output(print(summary_result))

  expect_false(any(grepl("Selected edges", output, fixed = TRUE)))
})
