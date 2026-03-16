test_that("new_cpm and new_cpm_resamples build expected objects", {
  withr::local_seed(99)
  conmat <- matrix(rnorm(120), nrow = 10, ncol = 12)
  behav <- rnorm(10)
  spec <- cpm_spec(thresh_method = "alpha", thresh_level = 0.05)
  call <- quote(fit(object = spec, conmat = conmat, behav = behav))

  fit_result <- run_single_fit(
    object = spec,
    conmat = conmat,
    behav = behav,
    covariates = NULL,
    na_action = "fail",
    call = call
  )

  cpm_object <- new_cpm(
    call = call,
    behav = behav,
    pred = fit_result$pred,
    edges = fit_result$edges,
    model = fit_result$model,
    spec = spec,
    params = fit_result$params
  )
  expect_s3_class(cpm_object, "cpm")
  expect_identical(cpm_object$call, call)
  expect_false("folds" %in% names(cpm_object))

  metrics <- data.frame(
    fold = 1L,
    n_assess = 2L,
    both = 0.5,
    pos = 0.4,
    neg = 0.3
  )
  predictions <- data.frame(
    row = 1:2,
    fold = 1L,
    real = c(1, 2),
    both = c(1.1, 1.9),
    pos = c(1.0, 2.0),
    neg = c(0.9, 2.1)
  )
  resamples_object <- new_cpm_resamples(
    spec = spec,
    folds = list(1:2),
    edges = fit_result$edges,
    metrics = metrics,
    predictions = predictions,
    params = list(return_edges = "sum")
  )
  expect_s3_class(resamples_object, "cpm_resamples")
  expect_identical(resamples_object$metrics, metrics)
  expect_identical(resamples_object$predictions, predictions)
})
