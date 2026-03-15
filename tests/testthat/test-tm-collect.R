test_that("collect_edges works on cpm_fit and rejects unrelated model_fit objects", {
  problem <- simulate_cpm_problem(n = 30, p = 12, seed = 61)
  fit <- core_fit_xy(
    conmat = problem$x,
    behav = problem$y,
    thresh_method = "sparsity",
    thresh_level = 0.2
  )

  expect_equal(collect_edges(fit), fit$edges)

  fake_fit <- structure(list(fit = 1), class = "model_fit")
  expect_error(
    collect_edges(fake_fit),
    "only supports `model_fit` objects created by the cpmr engine",
    fixed = FALSE
  )
})

test_that("extract_cpm_edges works on cpm_fit, model_fit, and workflow fits", {
  testthat::skip_if_not_installed("workflows")

  problem <- simulate_cpm_problem(n = 36, p = 14, seed = 62)
  spec <- cpm_reg(thresh_method = "sparsity", thresh_level = 0.2)

  fit_obj <- parsnip::fit(spec, y ~ ., data = problem$data[1:24, ])
  wf_fit <- workflows::workflow() |>
    workflows::add_formula(y ~ .) |>
    workflows::add_model(spec) |>
    workflows::fit(data = problem$data[1:24, ])

  extracted_fit <- extract_cpm_edges(fit_obj$fit)
  extracted_model_fit <- extract_cpm_edges(fit_obj)
  extracted_workflow <- extract_cpm_edges(wf_fit)

  expect_s3_class(extracted_fit, "cpm_edge_extract")
  expect_identical(extracted_fit$predictors, fit_obj$fit$predictors)
  expect_identical(extracted_fit$edges, fit_obj$fit$edges)
  expect_identical(extracted_model_fit$edges, fit_obj$fit$edges)
  expect_identical(extracted_workflow$edges, fit_obj$fit$edges)
})

test_that("collect_edges summarizes fit_resamples edge extracts", {
  testthat::skip_if_not_installed("rsample")
  testthat::skip_if_not_installed("tune")
  testthat::skip_if_not_installed("workflows")

  problem <- simulate_cpm_problem(n = 48, p = 16, seed = 63)
  spec <- cpm_reg(thresh_method = "sparsity", thresh_level = 0.2)

  wf <- workflows::workflow() |>
    workflows::add_formula(y ~ .) |>
    workflows::add_model(spec)

  res <- tune::fit_resamples(
    wf,
    resamples = rsample::vfold_cv(problem$data, v = 3),
    control = tune::control_resamples(extract = extract_cpm_edges)
  )

  edges_sum <- collect_edges(res, type = "sum")
  edges_prop <- collect_edges(res, type = "prop", selected_only = FALSE)
  edges_all <- collect_edges(res, type = "all")

  expect_named(edges_sum, c("predictor", "pos", "neg", "n_folds"))
  expect_true(all(edges_sum$n_folds == 3L))
  expect_true(all((edges_sum$pos > 0) | (edges_sum$neg > 0)))

  expect_named(edges_prop, c("predictor", "pos", "neg", "n_folds"))
  expect_equal(nrow(edges_prop), 16)
  expect_true(all(edges_prop$pos >= 0 & edges_prop$pos <= 1))
  expect_true(all(edges_prop$neg >= 0 & edges_prop$neg <= 1))

  expect_named(edges_all, c("id", "predictor", "pos", "neg"))
  expect_true(all(edges_all$pos | edges_all$neg))
})

test_that("collect_edges errors on resample results without edge extracts", {
  testthat::skip_if_not_installed("rsample")
  testthat::skip_if_not_installed("tune")
  testthat::skip_if_not_installed("workflows")

  problem <- simulate_cpm_problem(n = 36, p = 12, seed = 64)
  spec <- cpm_reg(thresh_method = "sparsity", thresh_level = 0.2)

  wf <- workflows::workflow() |>
    workflows::add_formula(y ~ .) |>
    workflows::add_model(spec)

  res <- tune::fit_resamples(
    wf,
    resamples = rsample::vfold_cv(problem$data, v = 3)
  )

  expect_error(
    collect_edges(res),
    "control_resamples\\(extract = extract_cpm_edges\\)",
    fixed = FALSE
  )
})

test_that("collect_edges keeps tuning metadata for tune_grid results", {
  testthat::skip_if_not_installed("rsample")
  testthat::skip_if_not_installed("tune")
  testthat::skip_if_not_installed("workflows")

  problem <- simulate_cpm_problem(n = 48, p = 16, seed = 65)
  spec <- cpm_reg(
    thresh_method = "sparsity",
    thresh_level = tune::tune(),
    network = "both"
  )

  wf <- workflows::workflow() |>
    workflows::add_formula(y ~ .) |>
    workflows::add_model(spec)

  res <- tune::tune_grid(
    wf,
    resamples = rsample::vfold_cv(problem$data, v = 3),
    grid = tibble::tibble(thresh_level = c(0.15, 0.25)),
    control = tune::control_grid(extract = extract_cpm_edges)
  )

  edges_sum <- collect_edges(res, type = "sum")
  edges_all <- collect_edges(res, type = "all", selected_only = FALSE)

  expect_named(
    edges_sum,
    c("thresh_level", ".config", "predictor", "pos", "neg", "n_folds")
  )
  expect_true(all(c(0.15, 0.25) %in% edges_sum$thresh_level))
  expect_true(all(edges_sum$n_folds == 3L))

  expect_named(
    edges_all,
    c("thresh_level", ".config", "id", "predictor", "pos", "neg")
  )
  expect_true(all(c(0.15, 0.25) %in% edges_all$thresh_level))
})
