test_that("Different threshold methods works", {
  withr::local_seed(123)
  conmat <- matrix(rnorm(100), ncol = 10)
  behav <- rnorm(10)
  cpm(conmat, behav, thresh_method = "alpha") |>
    expect_snapshot()
  cpm(conmat, behav, thresh_method = "sparsity") |>
    expect_snapshot()
})

test_that("Different threshold levels works", {
  withr::local_seed(123)
  conmat <- matrix(rnorm(100), ncol = 10)
  behav <- rnorm(10)
  cpm(conmat, behav, thresh_level = 0.1) |>
    expect_snapshot()
  cpm(conmat, behav, thresh_level = 0.2) |>
    expect_snapshot()
})

test_that("Works with confounds", {
  withr::local_seed(123)
  conmat <- matrix(rnorm(100), ncol = 10)
  behav <- rnorm(10)
  confounds <- rnorm(10)
  cpm(conmat, behav, confounds = confounds) |>
    expect_snapshot()
})

test_that("Keep names of behavior", {
  withr::local_seed(123)
  conmat <- matrix(rnorm(100), ncol = 10)
  behav <- rnorm(10)
  names(behav) <- LETTERS[1:10]
  cpm(conmat, behav) |>
    expect_snapshot()
})

test_that("Check observation names match", {
  withr::local_seed(123)
  conmat <- matrix(rnorm(100), ncol = 10)
  rownames(conmat) <- letters[1:10]
  behav <- rnorm(10)
  names(behav) <- LETTERS[1:10]
  cpm(conmat, behav) |>
    expect_error("do not match")
})
