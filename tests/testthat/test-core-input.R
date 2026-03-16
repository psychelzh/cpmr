test_that("normalize_inputs validates and normalizes data", {
  conmat <- matrix(rnorm(20), ncol = 2)
  behav <- matrix(rnorm(10), ncol = 1)
  covariates <- rnorm(10)

  normalized <- normalize_inputs(conmat, behav, covariates)

  expect_true(is.vector(normalized$behav))
  expect_equal(length(normalized$behav), nrow(conmat))
  expect_true(is.matrix(normalized$covariates))
  expect_equal(nrow(normalized$covariates), nrow(conmat))
})

test_that("resolve_include_cases returns intersection in exclude mode", {
  conmat <- matrix(rnorm(30), ncol = 3)
  behav <- rnorm(10)
  covariates <- matrix(rnorm(10), ncol = 1)

  conmat[1, 1] <- NA
  behav[2] <- NA
  covariates[3, 1] <- NA

  include_cases <- resolve_include_cases(
    conmat,
    behav,
    covariates,
    na_action = "exclude"
  )

  expect_identical(include_cases, 4:10)
})

test_that("resolve_kfolds uses complete-case count when NULL", {
  expect_identical(resolve_kfolds(NULL, include_cases = c(2L, 4L, 7L)), 3L)
  expect_identical(resolve_kfolds(5L, include_cases = c(2L, 4L, 7L)), 5L)
})

test_that("init_edges allocates expected structures", {
  conmat <- matrix(rnorm(40), ncol = 4)

  edges_sum <- init_edges("sum", conmat, kfolds = 5)
  expect_equal(dim(edges_sum), c(ncol(conmat), 2))
  expect_identical(colnames(edges_sum), c("pos", "neg"))

  edges_all <- init_edges("all", conmat, kfolds = 5)
  expect_equal(dim(edges_all), c(ncol(conmat), 2, 5))

  expect_null(init_edges("none", conmat, kfolds = 5))
})

test_that("init_pred preserves prediction matrix structure", {
  behav <- stats::setNames(rnorm(5), paste0("s", 1:5))

  pred <- init_pred(behav)

  expect_equal(dim(pred), c(5, 3))
  expect_identical(rownames(pred), names(behav))
  expect_identical(colnames(pred), c("both", "pos", "neg"))
})
