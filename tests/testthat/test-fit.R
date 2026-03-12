test_that("cpm_spec stores model parameters", {
  spec <- cpm_spec(
    thresh_method = "sparsity",
    thresh_level = 0.05,
    kfolds = 5,
    bias_correct = FALSE,
    return_edges = "none",
    na_action = "exclude"
  )

  expect_s3_class(spec, "cpm_spec")
  expect_identical(spec$params$thresh_method, "sparsity")
  expect_identical(spec$params$thresh_level, 0.05)
  expect_identical(spec$params$kfolds, 5)
  expect_false(spec$params$bias_correct)
  expect_identical(spec$params$return_edges, "none")
  expect_identical(spec$params$na_action, "exclude")
})

test_that("fit.cpm_spec matches cpm workflow", {
  conmat <- matrix(rnorm(100), ncol = 10)
  behav <- rnorm(10)
  spec <- cpm_spec(kfolds = 5, return_edges = "sum")

  withr::local_seed(123)
  fitted_spec <- fit(spec, conmat = conmat, behav = behav)

  withr::local_seed(123)
  fitted_cpm <- cpm(conmat, behav, kfolds = 5, return_edges = "sum")

  expect_s3_class(fitted_spec, "cpm")
  expect_identical(fitted_spec$real, fitted_cpm$real)
  expect_identical(fitted_spec$pred, fitted_cpm$pred)
  expect_identical(fitted_spec$edges, fitted_cpm$edges)
  expect_identical(fitted_spec$params, fitted_cpm$params)
})

test_that("fit.cpm_spec and cpm preserve entry-point call", {
  conmat <- matrix(rnorm(100), ncol = 10)
  behav <- rnorm(10)

  withr::local_seed(123)
  fitted_spec <- fit(cpm_spec(kfolds = 5), conmat = conmat, behav = behav)

  withr::local_seed(123)
  fitted_cpm <- cpm(conmat, behav, kfolds = 5)

  expect_identical(as.character(fitted_spec$call[[1]]), "fit")
  expect_identical(as.character(fitted_cpm$call[[1]]), "cpm")
})
