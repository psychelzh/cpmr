test_that("normalize_inputs validates and normalizes data", {
  normalize_inputs <- getFromNamespace("normalize_inputs", "cpmr")

  conmat <- matrix(rnorm(20), ncol = 2)
  behav <- matrix(rnorm(10), ncol = 1)
  confounds <- rnorm(10)

  normalized <- normalize_inputs(conmat, behav, confounds)

  expect_true(is.vector(normalized$behav))
  expect_equal(length(normalized$behav), nrow(conmat))
  expect_true(is.matrix(normalized$confounds))
  expect_equal(nrow(normalized$confounds), nrow(conmat))
})

test_that("resolve_include_cases returns intersection in exclude mode", {
  resolve_include_cases <- getFromNamespace("resolve_include_cases", "cpmr")

  conmat <- matrix(rnorm(30), ncol = 3)
  behav <- rnorm(10)
  confounds <- matrix(rnorm(10), ncol = 1)

  conmat[1, 1] <- NA
  behav[2] <- NA
  confounds[3, 1] <- NA

  include_cases <- resolve_include_cases(
    conmat,
    behav,
    confounds,
    na_action = "exclude"
  )

  expect_identical(include_cases, 4:10)
})

test_that("init_edges allocates expected structures", {
  init_edges <- getFromNamespace("init_edges", "cpmr")

  conmat <- matrix(rnorm(40), ncol = 4)

  edges_sum <- init_edges("sum", conmat, kfolds = 5)
  expect_equal(dim(edges_sum), c(ncol(conmat), 2))
  expect_identical(colnames(edges_sum), c("pos", "neg"))

  edges_all <- init_edges("all", conmat, kfolds = 5)
  expect_equal(dim(edges_all), c(ncol(conmat), 2, 5))

  expect_null(init_edges("none", conmat, kfolds = 5))
})

test_that("apply_confounds_regression is no-op when confounds are NULL", {
  apply_confounds_regression <-
    getFromNamespace("apply_confounds_regression", "cpmr")

  conmat <- matrix(rnorm(20), ncol = 2)
  behav <- rnorm(10)

  out <- apply_confounds_regression(conmat, behav, NULL, include_cases = 1:10)

  expect_identical(out$conmat, conmat)
  expect_identical(out$behav, behav)
})
