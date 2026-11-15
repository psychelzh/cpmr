test_that("normalize_manual_folds rejects malformed assessment sets", {
  expect_error(
    normalize_manual_folds(list(), include_cases = 1:4),
    "non-empty list",
    fixed = FALSE
  )
  expect_error(
    normalize_manual_folds(
      list(integer(), 1:4),
      include_cases = 1:4
    ),
    "must contain assessment indices",
    fixed = TRUE
  )
  expect_error(
    normalize_manual_folds(list(c(1, Inf), 2:3), include_cases = 1:4),
    "finite numeric indices",
    fixed = FALSE
  )
  expect_error(
    normalize_manual_folds(list(1:2, 5:6), include_cases = 1:4),
    "contained in complete-case rows",
    fixed = FALSE
  )
})

test_that("make_kfold_assessment_folds partitions inputs into non-overlapping folds", {
  withr::local_seed(123)
  folds <- make_kfold_assessment_folds(1:6, 3)

  expect_length(folds, 3)
  expect_identical(sort(unlist(folds, use.names = FALSE)), 1:6)
})

test_that("resolve_assessment_folds generates and validates public folds", {
  withr::local_seed(123)
  include_cases <- c(1L, 2L, 3L, 4L, 5L)

  resolved <- resolve_assessment_folds(
    resamples = 5,
    include_cases = include_cases
  )
  expect_identical(sort(unlist(resolved, use.names = FALSE)), include_cases)

  explicit <- resolve_assessment_folds(
    resamples = list(1L, 2L, 3L, 4L, 5L),
    include_cases = include_cases
  )
  expect_identical(explicit, list(1L, 2L, 3L, 4L, 5L))

  loo <- resolve_assessment_folds(
    resamples = NULL,
    include_cases = include_cases
  )
  expect_identical(sort(unlist(loo, use.names = FALSE)), include_cases)

  expect_error(
    resolve_assessment_folds(
      resamples = 2,
      include_cases = 1:4
    ),
    "must leave at least 3 complete-case observations for training",
    fixed = TRUE
  )
})

test_that("resolve_assessment_folds requires enough complete cases to define folds", {
  expect_error(
    resolve_assessment_folds(
      resamples = NULL,
      include_cases = 1L
    ),
    "CPM requires at least 2 complete-case observations to define assessment folds.",
    fixed = TRUE
  )
})

test_that("normalize_fold_count rejects malformed fold counts", {
  expect_error(
    normalize_fold_count(1),
    "`resamples` must be NULL, a single integer greater than or equal to 2, or a non-empty list of assessment indices.",
    fixed = TRUE
  )
  expect_error(
    normalize_fold_count(2.5),
    "`resamples` must be NULL, a single integer greater than or equal to 2, or a non-empty list of assessment indices.",
    fixed = TRUE
  )
  expect_identical(normalize_fold_count(4), 4L)
})
