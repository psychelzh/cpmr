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

test_that("init_edges allocates expected structures", {
  conmat <- matrix(rnorm(40), ncol = 4)

  edges_sum <- init_edges("sum", conmat, kfolds = 5)
  expect_equal(dim(edges_sum), c(ncol(conmat), 2))
  expect_identical(colnames(edges_sum), c("pos", "neg"))

  edges_all <- init_edges("all", conmat, kfolds = 5)
  expect_equal(dim(edges_all), c(ncol(conmat), 2, 5))

  expect_null(init_edges("none", conmat, kfolds = 5))
})

test_that("core scalar helpers return expected values", {
  behav <- stats::setNames(rnorm(5), paste0("s", 1:5))

  pred <- init_pred(behav)
  expect_equal(dim(pred), c(5, 3))
  expect_identical(rownames(pred), names(behav))
  expect_identical(colnames(pred), c("both", "pos", "neg"))

  df <- 20 - 2
  ct <- stats::qt(0.05 / 2, df, lower.tail = FALSE)
  expect_equal(
    critical_r(20, 0.05),
    sqrt((ct^2) / ((ct^2) + df))
  )

  x <- matrix(as.numeric(1:6), nrow = 3)
  center <- c(2, 5)
  scale <- c(1, 2)
  expect_equal(
    fscale(x, center, scale),
    sweep(sweep(x, 2, center, "-"), 2, scale, "/")
  )

  withr::local_seed(42)
  folds <- crossv_kfold(1:6, 3)
  expect_length(folds, 3)
  expect_identical(sort(unname(unlist(folds))), 1:6)
})
