test_that("core resample helpers resolve folds and return metrics", {
  problem <- simulate_cpm_problem(n = 36, p = 18, seed = 14)

  withr::local_seed(99)
  resolved <- core_resolve_resample_folds(
    resamples = NULL,
    kfolds = 6,
    include_cases = seq_len(nrow(problem$x))
  )

  expect_length(resolved$folds, 6)
  expect_identical(
    sort(unname(unlist(resolved$folds))),
    seq_len(nrow(problem$x))
  )

  res <- core_fit_resamples(
    conmat = problem$x,
    behav = problem$y,
    resamples = resolved$folds,
    thresh_method = "sparsity",
    thresh_level = 0.2,
    network = "both",
    return_edges = "sum"
  )

  expect_named(res, c("folds", "edges", "metrics", "predictions", "params"))
  expect_equal(nrow(res$metrics), 6)
  expect_equal(nrow(res$predictions), nrow(problem$x))
  expect_equal(dim(res$edges), c(ncol(problem$x), 2))
})

test_that("resample validation covers error paths and warnings", {
  include_cases <- 1:6

  expect_null(core_validate_kfolds(NULL))
  expect_equal(core_validate_kfolds(3), 3L)
  expect_error(
    core_validate_kfolds(1),
    "must be NULL or a single integer greater than or equal to 2",
    fixed = FALSE
  )
  expect_error(
    core_validate_resamples("bad", include_cases),
    "`resamples` must be a non-empty list of assessment indices.",
    fixed = TRUE
  )
  expect_error(
    core_validate_resamples(list(1:2), include_cases),
    "must contain at least 2 assessment sets",
    fixed = FALSE
  )
  expect_error(
    core_validate_resamples(list(c(1, NA), 3:6), include_cases),
    "finite numeric indices",
    fixed = FALSE
  )
  expect_error(
    core_validate_resamples(list(c(1, 2.5), 3:6), include_cases),
    "integer-valued indices",
    fixed = FALSE
  )
  expect_error(
    core_validate_resamples(list(c(0, 1), 2:6), include_cases),
    "positive indices",
    fixed = FALSE
  )
  expect_error(
    core_validate_resamples(list(c(1, 1), 2:6), include_cases),
    "must not contain duplicates",
    fixed = FALSE
  )
  expect_error(
    core_validate_resamples(list(1:3, 4:7), include_cases),
    "contained in complete-case rows",
    fixed = FALSE
  )
  expect_error(
    core_validate_resamples(list(1:3, 3:6), include_cases),
    "must not overlap across folds",
    fixed = FALSE
  )
  expect_error(
    core_validate_resamples(list(1:2, 3:4), include_cases),
    "must cover all complete-case rows exactly once",
    fixed = FALSE
  )

  expect_error(
    core_resolve_resample_folds(list(1:3, 4:6), 2, include_cases),
    "Specify either `resamples` or `kfolds`, not both.",
    fixed = TRUE
  )
  expect_error(
    core_resolve_resample_folds(NULL, 7, include_cases),
    "`kfolds` must be less than or equal to complete-case observations.",
    fixed = TRUE
  )
  expect_error(
    core_resolve_resample_folds(list(1:4, 5:6), NULL, include_cases),
    "leave at least 3 complete-case training observations",
    fixed = FALSE
  )
  expect_warning(
    core_warn_large_edge_storage(700000, 2, "all"),
    "large memory",
    fixed = FALSE
  )
})

test_that("core_fit_resamples covers edge-case failures and evaluation helpers", {
  expect_error(
    core_fit_resamples(
      conmat = matrix(c(NA_real_, NA_real_, NA_real_, NA_real_), nrow = 2),
      behav = c(1, 2),
      na_action = "exclude"
    ),
    "No complete-case observations available for resampling.",
    fixed = TRUE
  )
  expect_error(
    core_fit_resamples(
      conmat = matrix(c(1, 2, NA_real_, NA_real_), nrow = 2, byrow = TRUE),
      behav = c(1, 2),
      na_action = "exclude"
    ),
    "At least 2 complete-case observations are required for resampling.",
    fixed = TRUE
  )
  expect_error(
    core_validate_resamples(list(1:2, 2:3), include_cases = 1:3),
    "must not overlap",
    fixed = FALSE
  )

  problem <- simulate_cpm_problem(n = 18, p = 9, seed = 52)
  res <- core_fit_resamples(
    conmat = problem$x,
    behav = problem$y,
    kfolds = 3,
    thresh_method = "alpha",
    thresh_level = 0.1,
    return_edges = "all"
  )

  expect_equal(dim(res$edges), c(9, 2, 3))
  expect_equal(core_safe_cor(c(1), c(1)), NA_real_)
  expect_equal(core_safe_cor(c(1, 1, 1), c(1, 2, 3)), NA_real_)
})
