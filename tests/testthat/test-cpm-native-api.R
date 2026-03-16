test_that("cpm_fit matches fit(cpm_spec()) on the same inputs", {
  withr::local_seed(123)
  conmat <- matrix(rnorm(120), ncol = 12)
  behav <- rnorm(10)

  direct <- cpm_fit(
    conmat = conmat,
    behav = behav,
    thresh_method = "sparsity",
    thresh_level = 0.2,
    bias_correct = FALSE,
    na_action = "fail"
  )
  via_spec <- fit(
    cpm_spec(
      thresh_method = "sparsity",
      thresh_level = 0.2,
      bias_correct = FALSE
    ),
    conmat = conmat,
    behav = behav,
    return_edges = "sum",
    na_action = "fail"
  )

  expect_s3_class(direct, "cpm")
  expect_identical(as.character(direct$call[[1]]), "cpm_fit")
  expect_false("return_edges" %in% names(direct$params))
  expect_equal(direct$pred, via_spec$pred)
  expect_equal(direct$edges, via_spec$edges)
  expect_equal(direct$model$models, via_spec$model$models)
  expect_identical(direct$spec$params, via_spec$spec$params)
})

test_that("cpm_fit_resamples matches fit_resamples(cpm_spec())", {
  withr::local_seed(321)
  conmat <- matrix(rnorm(120), ncol = 12)
  behav <- rnorm(10)

  withr::local_seed(999)
  direct <- cpm_fit_resamples(
    conmat = conmat,
    behav = behav,
    kfolds = 5,
    thresh_method = "sparsity",
    thresh_level = 0.2,
    bias_correct = FALSE,
    na_action = "fail"
  )
  withr::local_seed(999)
  via_spec <- fit_resamples(
    cpm_spec(
      thresh_method = "sparsity",
      thresh_level = 0.2,
      bias_correct = FALSE
    ),
    conmat = conmat,
    behav = behav,
    kfolds = 5,
    return_edges = "sum",
    na_action = "fail"
  )

  expect_s3_class(direct, "cpm_resamples")
  expect_identical(direct$params$return_edges, "sum")
  expect_identical(direct$folds, via_spec$folds)
  expect_equal(direct$metrics, via_spec$metrics)
  expect_equal(direct$predictions, via_spec$predictions)
  expect_equal(direct$edges, via_spec$edges)
  expect_identical(direct$spec$params, via_spec$spec$params)
})
