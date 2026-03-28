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
