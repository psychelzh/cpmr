test_that("normalize_inputs validates and normalizes data", {
  normalize_inputs <- getFromNamespace("normalize_inputs", "cpmr")

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
  resolve_include_cases <- getFromNamespace("resolve_include_cases", "cpmr")

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
  init_edges <- getFromNamespace("init_edges", "cpmr")

  conmat <- matrix(rnorm(40), ncol = 4)

  edges_sum <- init_edges("sum", conmat, kfolds = 5)
  expect_equal(dim(edges_sum), c(ncol(conmat), 2))
  expect_identical(colnames(edges_sum), c("pos", "neg"))

  edges_all <- init_edges("all", conmat, kfolds = 5)
  expect_equal(dim(edges_all), c(ncol(conmat), 2, 5))

  expect_null(init_edges("none", conmat, kfolds = 5))
})

test_that("resolve_covariates handles deprecated alias", {
  resolve_covariates <- getFromNamespace("resolve_covariates", "cpmr")

  covariates <- matrix(rnorm(10), ncol = 1)
  expect_identical(resolve_covariates(covariates, NULL), covariates)
  expect_warning(
    resolve_covariates(NULL, covariates),
    "deprecated"
  )
  expect_error(
    resolve_covariates(covariates, covariates),
    "Please provide only one"
  )
})

test_that("print.cpm falls back to legacy confounds param", {
  old_obj <- structure(
    list(
      call = quote(cpm(conmat = conmat, behav = behav, confounds = confounds)),
      real = c(1, 2),
      pred = matrix(c(1, 2), ncol = 1),
      edges = NULL,
      folds = list(1, 2),
      params = list(
        confounds = TRUE,
        thresh_method = "alpha",
        thresh_level = 0.01,
        kfolds = 2,
        bias_correct = TRUE
      )
    ),
    class = "cpm"
  )

  expect_warning(
    output <- capture.output(print(old_obj)),
    "deprecated"
  )
  expect_true(any(grepl("Covariates:\\s+TRUE", output)))
})
