test_that("workflow with recipe fits and predicts through the cpmr engine", {
  testthat::skip_if_not_installed("recipes")
  testthat::skip_if_not_installed("workflows")

  problem <- simulate_cpm_problem(n = 50, p = 20, seed = 31)

  rec <- recipes::recipe(y ~ ., data = problem$data) |>
    recipes::step_normalize(recipes::all_predictors())

  wf <- workflows::workflow() |>
    workflows::add_recipe(rec) |>
    workflows::add_model(cpm_reg(
      thresh_method = "sparsity",
      thresh_level = 0.2
    ))

  wf_fit <- workflows::fit(wf, data = problem$data[1:40, ])
  pred <- predict(wf_fit, problem$data[41:50, ])

  expect_named(pred, ".pred")
  expect_equal(nrow(pred), 10)
  expect_equal(dim(collect_edges(wf_fit)), c(20, 2))
})
