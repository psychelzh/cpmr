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

test_that("fit_resamples works with grouped resampling and edge extraction", {
  testthat::skip_if_not_installed("rsample")
  testthat::skip_if_not_installed("tune")
  testthat::skip_if_not_installed("workflows")

  problem <- simulate_grouped_cpm_problem(
    n_subjects = 18,
    repeats = 2,
    p = 14,
    seed = 42
  )

  wf <- workflows::workflow() |>
    workflows::add_formula(y ~ . - subject_id) |>
    workflows::add_model(cpm_reg(
      thresh_method = "sparsity",
      thresh_level = 0.2,
      network = "both"
    ))

  folds <- rsample::group_vfold_cv(problem$data, group = subject_id, v = 3)
  res <- tune::fit_resamples(
    wf,
    resamples = folds,
    metrics = yardstick::metric_set(yardstick::rmse, cpm_cor),
    control = tune::control_resamples(extract = extract_cpm_edges)
  )

  metrics <- tune::collect_metrics(res)
  edges <- collect_edges(res, type = "sum")

  expect_true(all(c("rmse", "cpm_cor") %in% metrics$.metric))
  expect_named(edges, c("predictor", "pos", "neg", "n_folds"))
  expect_true(all(edges$n_folds == 3L))
})

test_that("cpm_reg fits naturally inside a nested CV workflow", {
  testthat::skip_if_not_installed("rsample")
  testthat::skip_if_not_installed("tune")
  testthat::skip_if_not_installed("workflows")

  problem <- simulate_grouped_cpm_problem(
    n_subjects = 15,
    repeats = 2,
    p = 12,
    seed = 43
  )

  wf <- workflows::workflow() |>
    workflows::add_formula(y ~ . - subject_id) |>
    workflows::add_model(cpm_reg(
      thresh_method = "sparsity",
      thresh_level = tune::tune(),
      network = "both"
    ))

  nested <- rsample::nested_cv(
    problem$data,
    outside = rsample::group_vfold_cv(group = subject_id, v = 3),
    inside = rsample::vfold_cv(v = 2)
  )

  inner_res <- tune::tune_grid(
    wf,
    resamples = nested$inner_resamples[[1]],
    grid = tibble::tibble(thresh_level = c(0.15, 0.25)),
    metrics = yardstick::metric_set(yardstick::rmse, cpm_cor)
  )
  best <- tune::select_best(inner_res, metric = "rmse")
  final_wf <- tune::finalize_workflow(wf, best)
  outer_fit <- workflows::fit(
    final_wf,
    data = rsample::analysis(nested$splits[[1]])
  )
  outer_pred <- predict(outer_fit, rsample::assessment(nested$splits[[1]]))

  expect_true("thresh_level" %in% names(best))
  expect_named(outer_pred, ".pred")
  expect_equal(nrow(outer_pred), nrow(rsample::assessment(nested$splits[[1]])))
  expect_equal(dim(collect_edges(outer_fit)), c(12, 2))
})
