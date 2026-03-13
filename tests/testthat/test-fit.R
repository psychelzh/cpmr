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

test_that("fit.cpm_spec returns a cpm object with correct call", {
  conmat <- matrix(rnorm(100), ncol = 10)
  behav <- rnorm(10)
  spec <- cpm_spec(kfolds = 5)
  result <- fit(spec, conmat = conmat, behav = behav)
  expect_s3_class(result, "cpm")
  expect_identical(as.character(result$call[[1]]), "fit")
})

test_that("print.cpm_spec shows auto folds when kfolds is NULL", {
  spec <- cpm_spec()

  expect_output(print(spec), "CPM model specification")
  expect_output(print(spec), "CV folds:         auto")
})

test_that("print.cpm_spec shows explicit folds when kfolds is set", {
  spec <- cpm_spec(kfolds = 5)

  expect_output(print(spec), "CV folds:         5")
})
