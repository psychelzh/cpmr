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
