test_that("tune_grid works with cpm_reg and cpm_cor", {
  testthat::skip_if_not_installed("recipes")
  testthat::skip_if_not_installed("rsample")
  testthat::skip_if_not_installed("tune")
  testthat::skip_if_not_installed("workflows")

  problem <- simulate_cpm_problem(n = 54, p = 18, seed = 41)

  spec <- cpm_reg(
    thresh_method = "sparsity",
    thresh_level = tune::tune(),
    network = "both"
  )

  wf <- workflows::workflow() |>
    workflows::add_formula(y ~ .) |>
    workflows::add_model(spec)

  folds <- rsample::vfold_cv(problem$data, v = 3)
  grid <- tibble::tibble(thresh_level = c(0.15, 0.25))

  res <- tune::tune_grid(
    wf,
    resamples = folds,
    grid = grid,
    metrics = yardstick::metric_set(yardstick::rmse, cpm_cor)
  )

  metrics <- tune::collect_metrics(res)

  expect_true(all(c("rmse", "cpm_cor") %in% metrics$.metric))
  expect_true(all(c(0.15, 0.25) %in% metrics$thresh_level))
})
