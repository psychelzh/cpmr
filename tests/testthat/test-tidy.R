single_fit_result <- function(
  spec = spec(),
  conmat,
  behav,
  covariates = NULL,
  na_action = "fail"
) {
  run_single_fit(
    object = spec,
    conmat = conmat,
    behav = behav,
    covariates = covariates,
    na_action = na_action,
    call = quote(cpm(conmat = conmat, behav = behav, spec = spec))
  )
}

test_that("Basic case works for `tidy()`", {
  withr::local_seed(123)
  conmat <- matrix(rnorm(100), ncol = 10)
  behav <- rnorm(10)
  result <- single_fit_result(spec(), conmat = conmat, behav = behav)

  performance <- tidy(result, component = "performance")
  edges <- tidy(result, component = "edges")

  expect_named(
    performance,
    c(
      "covariates",
      "na_action",
      "selection_type",
      "selection_method",
      "selection_criterion",
      "selection_level",
      "construction_type",
      "construction_sign_mode",
      "weight_scale",
      "standardize_edges",
      "model_type",
      "method",
      "joint",
      "positive",
      "negative"
    )
  )
  expect_named(
    edges,
    c(
      "covariates",
      "na_action",
      "selection_type",
      "selection_method",
      "selection_criterion",
      "selection_level",
      "construction_type",
      "construction_sign_mode",
      "weight_scale",
      "standardize_edges",
      "model_type",
      "positive",
      "negative"
    )
  )
})

test_that("Support pass arguments of `summary()`", {
  withr::local_seed(123)
  conmat <- matrix(rnorm(100), ncol = 10)
  behav <- rnorm(10)
  result <- single_fit_result(spec(), conmat = conmat, behav = behav)
  expect_equal(
    tidy(result, component = "performance", method = "spearman")$method,
    "spearman"
  )
  expect_named(
    tidy(result, component = "edges"),
    c(
      "covariates",
      "na_action",
      "selection_type",
      "selection_method",
      "selection_criterion",
      "selection_level",
      "construction_type",
      "construction_sign_mode",
      "weight_scale",
      "standardize_edges",
      "model_type",
      "positive",
      "negative"
    )
  )
})

test_that("tidy supports pooled and foldwise metric tables", {
  withr::local_seed(123)
  conmat <- matrix(rnorm(200), nrow = 20)
  behav <- rnorm(20)
  result <- cpm(conmat = conmat, behav = behav, spec = spec(), resamples = 4)

  foldwise <- tidy(result, component = "metrics")
  pooled <- tidy(result, component = "metrics", level = "pooled")

  expect_true(all(
    c("fold", "n_assess", "metric", "prediction", "estimate") %in%
      names(foldwise)
  ))
  expect_true(all(c("metric", "prediction", "estimate") %in% names(pooled)))
  expect_true(all(c("rmse", "mae", "correlation") %in% pooled$metric))
})

test_that("tidy edges errors clearly when edge storage is disabled", {
  withr::local_seed(123)
  conmat <- matrix(rnorm(100), ncol = 10)
  behav <- rnorm(10)
  result <- cpm(
    conmat = conmat,
    behav = behav,
    spec = spec(),
    resamples = 5,
    return_edges = "none"
  )

  expect_error(
    tidy(result, component = "edges"),
    paste0(
      "Edge output is unavailable because this `cpm` object was fit ",
      "with `return_edges = \"none\"`. Refit with `return_edges = ",
      "\"sum\"` or `\"all\"` to tidy edges."
    ),
    fixed = TRUE
  )
})
